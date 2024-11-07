import gleam/bool
import gleam/dict
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute.{class}
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import squared_away/renderable_error
import squared_away/squared_away_lang as lang
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/interpreter/value
import squared_away/squared_away_lang/typechecker/typed_expr

const initial_grid_width = 7

const initial_grid_height = 20

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

@external(javascript, "./squared_away_ffi.js", "focus")
fn focus(id: String) -> Nil

@external(javascript, "./squared_away_ffi.js", "saveFile")
fn save_file(content: String, filename: String) -> Nil

@external(javascript, "./squared_away_ffi.js", "uploadFile")
fn upload_file() -> promise.Promise(String)

/// To start, our model will be a 5x5 grid of Strings
type Model {
  Model(
    holding_shift: Bool,
    grid_width: Int,
    grid_height: Int,
    display_formulas: Bool,
    display_coords: Bool,
    active_cell: Option(grid.GridKey),
    src_grid: grid.Grid(String),
    value_grid: grid.Grid(Result(value.Value, error.CompileError)),
    errors_to_display: List(#(grid.GridKey, error.CompileError)),
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let src_grid = grid.new(initial_grid_width, initial_grid_height, "")
  let value_grid =
    grid.new(initial_grid_width, initial_grid_height, Ok(value.Empty))

  let model =
    Model(
      holding_shift: False,
      grid_width: initial_grid_width,
      grid_height: initial_grid_height,
      display_formulas: False,
      display_coords: False,
      active_cell: None,
      src_grid:,
      value_grid:,
      errors_to_display: [],
    )
    |> update_grid

  #(model, effect.none())
}

type Msg {
  Noop
  UserToggledFormulaMode(to: Bool)
  UserToggledDisplayCoords(to: Bool)
  UserSetCellValue(key: grid.GridKey, val: String)
  UserFocusedOnCell(key: grid.GridKey)
  UserFocusedOffCell
  UserHitKeyInCell(key: grid.GridKey, keyboard_key: String)
  UserReleasedKeyInCell(keyboard_key: String)
  UserClickedSaveBtn
  UserUploadedFile(path: String)
  FileUploadComplete(file_content: String)
}

fn update_grid(model: Model) -> Model {
  let scanned = lang.scan_grid(model.src_grid)
  let parsed = lang.parse_grid(scanned)
  let typechecked = lang.typecheck_grid(parsed)
  let value_grid = lang.interpret_grid(typechecked)

  // Loop over the grid to see if there's any errors to display
  let errors_to_display =
    grid.fold(value_grid, [], fn(acc, key, val) {
      case val {
        Error(err) -> [#(key, err), ..acc]
        Ok(_) -> acc
      }
    })

  Model(..model, value_grid:, errors_to_display:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Noop -> #(model, effect.none())
    UserSetCellValue(key, val) -> {
      let model =
        Model(..model, src_grid: grid.insert(model.src_grid, key, val))
      #(update_grid(model), effect.none())
    }
    UserToggledFormulaMode(display_formulas) -> {
      #(Model(..model, display_formulas:), effect.none())
    }
    UserToggledDisplayCoords(display_coords) -> {
      #(Model(..model, display_coords:), effect.none())
    }
    UserFocusedOnCell(key) -> {
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
    UserFocusedOffCell -> {
      #(Model(..model, active_cell: None), effect.none())
    }
    UserHitKeyInCell(key, keyboard_key) -> {
      case keyboard_key, model.holding_shift {
        "Shift", False -> #(Model(..model, holding_shift: True), effect.none())
        "ArrowUp", _ ->
          set_active_cell_to(model, grid.cell_above(model.src_grid, key))
        "ArrowLeft", _ ->
          set_active_cell_to(model, grid.cell_to_the_left(model.src_grid, key))
        "Enter", _ | "ArrowDown", False ->
          set_active_cell_to(model, grid.cell_underneath(model.src_grid, key))
        "ArrowRight", False ->
          set_active_cell_to(model, grid.cell_to_the_right(model.src_grid, key))
        "ArrowRight", True -> {
          let maybe_cell_to_right = grid.cell_to_the_right(model.src_grid, key)
          case maybe_cell_to_right {
            Error(Nil) -> #(model, effect.none())
            Ok(cell_to_right) -> {
              // Alright, this might be a slightly complex operation
              // If the current formula is a cross label, we want to 
              // produce the equivalent cross label but updated to the new
              // column value.

              let scanned = lang.scan_grid(model.src_grid)
              let parsed = lang.parse_grid(scanned)
              let typechecked = lang.typecheck_grid(parsed)
              let maybe_expr = grid.get(typechecked, key)

              // if it doesn't typecheck, don't copy it over
              use <- bool.guard(maybe_expr |> result.is_error, #(
                model,
                effect.none(),
              ))
              // This assertion is safe because of the above bool.guard check
              let assert Ok(expr) = maybe_expr

              let expr_with_labels_updated =
                typed_expr.visit_cross_labels(
                  expr,
                  fn(key, row_label, col_label) {
                    // For this case, we want to get the label directly to the right of the col label

                    // This assertion is safe because we know the label is present in the grid,
                    // because we got it from a typechecked cross_label
                    let assert Ok(key_for_col) =
                      grid.find(model.src_grid, col_label)

                    use key_for_new_col <- result.try(grid.cell_to_the_right(
                      model.src_grid,
                      key_for_col,
                    ))

                    let maybe_new_label = grid.get(typechecked, key_for_new_col)
                    let get_new_label = fn(
                      l: Result(typed_expr.TypedExpr, error.CompileError),
                    ) {
                      case l {
                        Ok(typed_expr.LabelDef(_, txt)) -> Ok(txt)
                        _ -> Error(Nil)
                      }
                    }
                    use new_label <- result.try(get_new_label(maybe_new_label))

                    // There was a cell to the right of the column label, so there 
                    // must be a cell to the right of this one as well
                    let assert Ok(new_key) =
                      grid.cell_to_the_right(model.src_grid, key)

                    Ok(typed_expr.CrossLabel(
                      expr.type_,
                      new_key,
                      row_label,
                      new_label,
                    ))
                  },
                )

              case expr_with_labels_updated {
                Error(_) -> #(model, effect.none())
                Ok(new_expr) -> {
                  let formula = "=" <> typed_expr.to_string(new_expr)
                  let src_grid =
                    grid.insert(model.src_grid, cell_to_right, formula)
                  let id = grid.to_string(cell_to_right)
                  focus(id)
                  let new_model =
                    Model(..model, src_grid:, active_cell: Some(cell_to_right))
                  #(update_grid(new_model), effect.none())
                }
              }
            }
          }
        }
        "ArrowDown", True -> {
          let maybe_cell_below = grid.cell_underneath(model.src_grid, key)
          case maybe_cell_below {
            Error(Nil) -> #(model, effect.none())
            Ok(cell_below) -> {
              let scanned = lang.scan_grid(model.src_grid)
              let parsed = lang.parse_grid(scanned)
              let typechecked = lang.typecheck_grid(parsed)
              let maybe_expr = grid.get(typechecked, key)

              // if it doesn't typecheck, don't copy it over
              use <- bool.guard(maybe_expr |> result.is_error, #(
                model,
                effect.none(),
              ))
              let assert Ok(expr) = maybe_expr

              let expr_with_labels_updated =
                typed_expr.visit_cross_labels(
                  expr,
                  fn(key, row_label, col_label) {
                    // For this case, we want to get the label directly below the row label
                    let assert Ok(key_for_row) =
                      grid.find(model.src_grid, row_label)

                    use key_for_new_row <- result.try(grid.cell_underneath(
                      model.src_grid,
                      key_for_row,
                    ))

                    let maybe_new_label = grid.get(typechecked, key_for_new_row)
                    let get_new_label = fn(
                      l: Result(typed_expr.TypedExpr, error.CompileError),
                    ) {
                      case l {
                        Ok(typed_expr.LabelDef(_, txt)) -> Ok(txt)
                        _ -> Error(Nil)
                      }
                    }
                    use new_label <- result.try(get_new_label(maybe_new_label))

                    // We know there's a cell underneath because there was a cell underneath the row label
                    let assert Ok(new_key) =
                      grid.cell_underneath(model.src_grid, key)

                    Ok(typed_expr.CrossLabel(
                      expr.type_,
                      new_key,
                      new_label,
                      col_label,
                    ))
                  },
                )

              case expr_with_labels_updated {
                Error(_) -> #(model, effect.none())
                Ok(new_expr) -> {
                  let formula = "=" <> typed_expr.to_string(new_expr)

                  let src_grid =
                    grid.insert(model.src_grid, cell_below, formula)
                  let id = grid.to_string(cell_below)
                  focus(id)
                  let new_model =
                    Model(..model, src_grid:, active_cell: Some(cell_below))
                  #(update_grid(new_model), effect.none())
                }
              }
            }
          }
        }
        _, _ -> #(model, effect.none())
      }
    }
    UserReleasedKeyInCell(keyboard_key) -> {
      case keyboard_key {
        "Shift" -> #(Model(..model, holding_shift: False), effect.none())
        _ -> #(model, effect.none())
      }
    }
    UserClickedSaveBtn -> {
      let content = grid.src_csv(model.src_grid)
      save_file(content, "myspreadsheet.csv")

      #(model, effect.none())
    }
    UserUploadedFile(_) -> {
      let get_file_contents_effect =
        effect.from(fn(dispatch) {
          promise.await(upload_file(), fn(file_content) {
            promise.new(fn(resolve) {
              dispatch(FileUploadComplete(file_content))
              resolve(Nil)
            })
          })
          Nil
        })

      #(model, get_file_contents_effect)
    }
    FileUploadComplete(file_content) -> {
      let src_grid =
        file_content
        |> grid.from_src_csv(initial_grid_width, initial_grid_height)
      let new_model = Model(..model, src_grid:) |> update_grid
      #(new_model, effect.none())
    }
  }
}

fn set_active_cell_to(model, key: Result(grid.GridKey, Nil)) {
  case key {
    Error(_) -> #(model, effect.none())
    Ok(key) -> {
      let id = grid.to_string(key)
      focus(id)
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
  }
}

fn view(model: Model) -> element.Element(Msg) {
  let error_to_display =
    list.find_map(model.errors_to_display, fn(e) {
      case Some(e.0) == model.active_cell {
        False -> Error(Nil)
        True -> Ok(error_view(e.1 |> error.to_renderable_error))
      }
    })
    |> result.unwrap(or: html.div([], []))

  let rows =
    model.src_grid.cells
    |> list.group(grid.row)
    |> dict.map_values(fn(_, keys) {
      let cells =
        list.sort(keys, fn(k1, k2) {
          int.compare(k1 |> grid.col(), k2 |> grid.col())
        })
        |> list.map(fn(key) {
          let on_keydown = event.on_keydown(UserHitKeyInCell(key, _))
          let on_keyup = event.on_keyup(UserReleasedKeyInCell)
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let id = attribute.id(grid.to_string(key))
          let value =
            case model.display_formulas, model.active_cell == Some(key) {
              True, _ | False, True -> grid.get(model.src_grid, key)
              False, _ ->
                case grid.get(model.value_grid, key) {
                  Error(_) -> grid.get(model.src_grid, key)
                  Ok(v) -> value.value_to_string(v)
                }
            }
            |> attribute.value

          let cell_is_errored =
            list.any(model.errors_to_display, fn(i) { i.0 == key })
          let error_class = case cell_is_errored {
            False -> attribute.none()
            True -> attribute.class("errorcell")
          }

          let #(color, background_color) = case
            grid.get(model.value_grid, key)
          {
            Error(_) -> #("#b30000", "#ffe6e6")
            Ok(v) ->
              case v {
                value.Text(_) -> #("#4a4a4a", "#f2f2f2")
                value.TestPass -> #("#006400", "#e6ffe6")
                value.TestFail -> #("#b30000", "#ffe6e6")
                _ -> #("black", "white")
              }
          }

          let input =
            html.input([
              on_input,
              on_focus,
              out_of_focus,
              value,
              on_keydown,
              on_keyup,
              id,
              attribute.type_("text"),
              error_class,
              attribute.style([
                #("background-color", background_color),
                #("color", color),
              ]),
            ])

          case model.display_coords {
            False -> html.td([], [input])
            True ->
              html.td([], [
                html.label([], t(grid.to_string(key) <> ": ")),
                input,
              ])
          }
        })

      html.tr([], cells)
    })
    |> dict.to_list
    |> list.sort(fn(r1, r2) { int.compare(r1.0, r2.0) })
    |> list.map(fn(e) { e.1 })

  let grid =
    html.div([class("table-container")], [
      html.table([attribute.class("tg")], [html.tbody([], rows)]),
    ])

  let formula_mode_toggle =
    html.input([
      attribute.type_("checkbox"),
      attribute.id("formula_mode"),
      event.on_check(UserToggledFormulaMode),
    ])

  let formula_mode_toggle_label =
    html.label([attribute.for("formula_mode")], t("Show formulas"))

  let grid_mode_toggle =
    html.input([
      attribute.type_("checkbox"),
      attribute.id("grid_mode"),
      event.on_check(UserToggledDisplayCoords),
    ])

  let grid_mode_toggle_label =
    html.label([attribute.for("grid_mode")], t("Show grid coordinates"))

  let save_button = html.button([event.on_click(UserClickedSaveBtn)], t("Save"))
  let load_label = html.label([], t("Load file"))
  let load_button =
    html.input([
      attribute.type_("file"),
      attribute.id("csvupload"),
      event.on_input(UserUploadedFile),
    ])

  html.div([], [
    formula_mode_toggle,
    formula_mode_toggle_label,
    grid_mode_toggle,
    grid_mode_toggle_label,
    save_button,
    load_label,
    load_button,
    html.br([]),
    html.br([]),
    grid,
    html.br([]),
    error_to_display,
  ])
}

fn error_view(re: renderable_error.RenderableError) {
  html.div(
    [
      attribute.style([
        #("background-color", "#ffe6e6"),
        #("color", "#b30000"),
        #("padding", "20px"),
        #("border-radius", "20px"),
      ]),
    ],
    [
      html.h4([], t(re.title)),
      html.p([], t(re.info)),
      ..case re.hint {
        None -> []
        Some(hint) -> [html.p([], t(hint))]
      }
    ],
  )
}

fn t(input: String) {
  [html.text(input)]
}
