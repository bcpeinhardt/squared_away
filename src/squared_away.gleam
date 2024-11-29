import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import squared_away/renderable_error
import squared_away/squared_away_lang
import squared_away/squared_away_lang as lang
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/interpreter/value
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/typechecker/typed_expr

const initial_grid_width = 30

const initial_grid_height = 40

const min_cell_size_ch = 10

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

@external(javascript, "./squared_away_ffi.js", "focus")
fn do_focus(_id: String) -> Nil {
  Nil
}

fn focus(id: String) -> effect.Effect(msg) {
  use _ <- effect.from
  do_focus(id)
}

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
    show_test_coverage: Bool,
    display_formulas: Bool,
    active_cell: Option(grid.GridKey),
    src_grid: grid.Grid(String),
    type_checked_grid: grid.Grid(
      Result(typed_expr.TypedExpr, error.CompileError),
    ),
    value_grid: grid.Grid(Result(value.Value, error.CompileError)),
    errors_to_display: List(#(grid.GridKey, error.CompileError)),
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let src_grid = grid.new(initial_grid_width, initial_grid_height, "")
  let type_checked_grid =
    grid.new(
      initial_grid_width,
      initial_grid_height,
      Ok(typed_expr.Empty(typ.TNil)),
    )
  let value_grid =
    grid.new(initial_grid_width, initial_grid_height, Ok(value.Empty))

  let model =
    Model(
      holding_shift: False,
      grid_width: initial_grid_width,
      grid_height: initial_grid_height,
      display_formulas: False,
      show_test_coverage: False,
      active_cell: None,
      src_grid:,
      value_grid:,
      type_checked_grid:,
      errors_to_display: [],
    )
    |> update_grid

  #(model, effect.none())
}

fn recalculate_col_width(model: Model, col: Int) -> Int {
  case model.display_formulas {
    False -> {
      // Based on value grid 
      grid.to_list(model.value_grid)
      |> list.filter_map(fn(c) {
        let #(k, v) = c
        case k |> grid.col == col {
          False -> Error(Nil)
          True ->
            case v {
              Error(_) -> Ok(grid.get(model.src_grid, k))
              Ok(v) ->
                case model.active_cell == Some(k) {
                  False -> Ok(value.value_to_string(v))
                  True -> Ok(grid.get(model.src_grid, k))
                }
            }
        }
      })
      |> list.map(string.length)
    }
    True -> {
      // based on the src grid 
      grid.to_list(model.src_grid)
      |> list.filter_map(fn(c) {
        let #(k, v) = c
        case k |> grid.col == col {
          False -> Error(Nil)
          True -> Ok(string.length(v))
        }
      })
    }
  }
  |> list.fold(min_cell_size_ch - 1, int.max)
  |> int.add(1)
}

type Msg {
  UserToggledShowTestCoverage(to: Bool)
  UserToggledFormulaMode(to: Bool)
  UserSetCellValue(key: grid.GridKey, val: String)
  UserFocusedOnCell(key: grid.GridKey)
  UserFocusedOffCell
  UserClickedSaveBtn
  UserUploadedFile(path: String)
  FileUploadComplete(file_content: String)
  UserPressedArrowUp(cell: grid.GridKey)
  UserPressedArrowLeft(cell: grid.GridKey)
  UserPressedArrowRight(cell: grid.GridKey)
  UserPressedArrowDown(cell: grid.GridKey)
  UserPressedEnter(cell: grid.GridKey)
  UserShiftPressedArrowRight(cell: grid.GridKey)
  UserShiftPressedArrowDown(cell: grid.GridKey)
}

fn update_grid(model: Model) -> Model {
  let scanned = lang.scan_grid(model.src_grid)
  let parsed = lang.parse_grid(scanned)
  let type_checked_grid = lang.typecheck_grid(parsed)
  let value_grid = lang.interpret_grid(type_checked_grid)

  // Loop over the grid to see if there's any errors to display
  let errors_to_display =
    grid.fold(value_grid, [], fn(acc, key, val) {
      case val {
        Error(err) -> [#(key, err), ..acc]
        Ok(_) -> acc
      }
    })

  Model(..model, value_grid:, type_checked_grid:, errors_to_display:)
}

// https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent#specifications
fn key_press_event(event, cell) {
  use key <- result.try(dynamic.field("key", dynamic.string)(event))
  use shift <- result.try(dynamic.field("shiftKey", dynamic.bool)(event))

  case key {
    "ArrowUp" -> Ok(UserPressedArrowUp(cell))
    "ArrowLeft" -> Ok(UserPressedArrowLeft(cell))
    "ArrowRight" if shift -> Ok(UserShiftPressedArrowRight(cell))
    "ArrowRight" -> Ok(UserPressedArrowRight(cell))
    "ArrowDown" if shift -> Ok(UserShiftPressedArrowDown(cell))
    "ArrowDown" -> Ok(UserPressedArrowDown(cell))
    "Enter" -> Ok(UserPressedEnter(cell))

    // Returning an empty error here means no message is dispatched at all.
    _ -> Error([])
  }
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserSetCellValue(key, val) -> {
      let old = grid.get(model.src_grid, key)

      // For every typed_expr that references the label, we need to update the source to be the serialized version
      let model =
        list.fold(model.type_checked_grid |> grid.to_list, model, fn(acc, g) {
          let #(k, te) = g
          case te {
            Error(_) -> acc
            Ok(te) -> {
              let new =
                typed_expr.update_labels(te, old, val) |> typed_expr.to_string
              Model(..acc, src_grid: grid.insert(acc.src_grid, k, new))
            }
          }
        })
      let model =
        Model(..model, src_grid: grid.insert(model.src_grid, key, val))

      let model = update_grid(model)

      #(model, effect.none())
    }
    UserToggledFormulaMode(display_formulas) -> {
      #(Model(..model, display_formulas:), effect.none())
    }
    UserToggledShowTestCoverage(show_test_coverage) -> {
      #(Model(..model, show_test_coverage:), effect.none())
    }
    UserFocusedOnCell(key) -> {
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
    UserFocusedOffCell -> {
      #(Model(..model, active_cell: None), effect.none())
    }
    UserPressedArrowUp(cell) ->
      set_active_cell_to(model, grid.cell_above(model.src_grid, cell))
    UserPressedArrowLeft(cell) ->
      set_active_cell_to(model, grid.cell_to_the_left(model.src_grid, cell))
    UserPressedArrowRight(cell) ->
      set_active_cell_to(model, grid.cell_to_the_right(model.src_grid, cell))
    UserPressedArrowDown(cell) ->
      set_active_cell_to(model, grid.cell_underneath(model.src_grid, cell))
    UserPressedEnter(cell) ->
      set_active_cell_to(model, grid.cell_underneath(model.src_grid, cell))
    UserShiftPressedArrowRight(cell) -> {
      let maybe_cell_to_right = grid.cell_to_the_right(model.src_grid, cell)
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
          let maybe_expr = grid.get(typechecked, cell)

          // if it doesn't typecheck, don't copy it over
          use <- bool.guard(maybe_expr |> result.is_error, #(
            model,
            effect.none(),
          ))
          // This assertion is safe because of the above bool.guard check
          let assert Ok(expr) = maybe_expr

          let expr_with_labels_updated =
            typed_expr.visit_cross_labels(expr, fn(key, row_label, col_label) {
              // For this case, we want to get the label directly to the right of the col label

              // This assertion is safe because we know the label is present in the grid,
              // because we got it from a typechecked cross_label
              let assert Ok(key_for_col) = grid.find(model.src_grid, col_label)

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
            })

          case expr_with_labels_updated {
            Error(_) -> #(model, effect.none())
            Ok(new_expr) -> {
              let formula = typed_expr.to_string(new_expr)
              let src_grid = grid.insert(model.src_grid, cell_to_right, formula)
              let id = grid.to_string(cell_to_right)
              let new_model =
                Model(..model, src_grid:, active_cell: Some(cell_to_right))
              #(update_grid(new_model), focus(id))
            }
          }
        }
      }
    }
    UserShiftPressedArrowDown(cell) -> {
      let maybe_cell_below = grid.cell_underneath(model.src_grid, cell)
      case maybe_cell_below {
        Error(Nil) -> #(model, effect.none())
        Ok(cell_below) -> {
          let scanned = lang.scan_grid(model.src_grid)
          let parsed = lang.parse_grid(scanned)
          let typechecked = lang.typecheck_grid(parsed)
          let maybe_expr = grid.get(typechecked, cell)

          // if it doesn't typecheck, don't copy it over
          use <- bool.guard(maybe_expr |> result.is_error, #(
            model,
            effect.none(),
          ))
          let assert Ok(expr) = maybe_expr

          let expr_with_labels_updated =
            typed_expr.visit_cross_labels(expr, fn(key, row_label, col_label) {
              // For this case, we want to get the label directly below the row label
              let assert Ok(key_for_row) = grid.find(model.src_grid, row_label)

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
              let assert Ok(new_key) = grid.cell_underneath(model.src_grid, key)

              Ok(typed_expr.CrossLabel(
                expr.type_,
                new_key,
                new_label,
                col_label,
              ))
            })

          case expr_with_labels_updated {
            Error(_) -> #(model, effect.none())
            Ok(new_expr) -> {
              let formula = typed_expr.to_string(new_expr)

              let src_grid = grid.insert(model.src_grid, cell_below, formula)
              let id = grid.to_string(cell_below)
              let new_model =
                Model(..model, src_grid:, active_cell: Some(cell_below))
              #(update_grid(new_model), focus(id))
            }
          }
        }
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

fn set_active_cell_to(model: Model, key: Result(grid.GridKey, Nil)) {
  case key {
    Error(_) -> #(model, effect.none())
    Ok(key) -> {
      let id = grid.to_string(key)
      #(Model(..model, active_cell: Some(key)), focus(id))
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

  let col_widths =
    list.range(1, initial_grid_width)
    |> list.map(fn(c) { #(c, recalculate_col_width(model, c)) })
    |> dict.from_list

  // In show test coverage mode, we need to check the dependency lists of *all*
  // passing tests
  let deps =
    model.type_checked_grid
    |> grid.to_list
    |> list.filter_map(fn(g) {
      let #(k, mte) = g
      case mte {
        Error(_) -> Error(Nil)
        Ok(te) ->
          case te.type_ {
            typ.TTestResult ->
              case grid.get(model.value_grid, k) {
                Ok(value.TestPass) -> Ok(te)
                _ -> Error(Nil)
              }
            _ -> Error(Nil)
          }
      }
    })
    |> list.map(squared_away_lang.dependency_list(
      model.type_checked_grid,
      _,
      [],
    ))
    |> list.flatten

  let rows =
    model.src_grid.cells
    |> list.group(grid.row)
    |> dict.map_values(fn(_, keys) {
      let cells =
        list.sort(keys, fn(k1, k2) {
          int.compare(k1 |> grid.col(), k2 |> grid.col())
        })
        |> list.map(fn(key) {
          let on_keydown = event.on("keydown", key_press_event(_, key))
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

          let alignment = case
            model.active_cell == Some(key) || model.display_formulas
          {
            True ->
              case grid.get(model.type_checked_grid, key) {
                Error(_) -> "left"
                Ok(te) ->
                  case te {
                    typed_expr.PercentLiteral(_, _)
                    | typed_expr.BooleanLiteral(_, _)
                    | typed_expr.UsdLiteral(_, _)
                    | typed_expr.IntegerLiteral(_, _)
                    | typed_expr.FloatLiteral(_, _) -> "right"
                    _ -> "left"
                  }
              }
            False ->
              case grid.get(model.value_grid, key) {
                Error(_) -> "left"
                Ok(v) ->
                  case v {
                    value.Percent(_)
                    | value.Integer(_)
                    | value.FloatingPointNumber(_)
                    | value.Usd(_)
                    | value.Boolean(_) -> "right"
                    value.TestFail | value.TestPass | value.Empty -> "center"
                    value.Text(_) -> "left"
                  }
              }
          }

          let cell_is_errored =
            list.any(model.errors_to_display, fn(i) { i.0 == key })
          let error_class = case cell_is_errored {
            False -> attribute.none()
            True -> attribute.class("errorcell")
          }

          let colors = case grid.get(model.value_grid, key) {
            Error(_) -> #("#b30000", "#ffe6e6")
            Ok(v) ->
              case v {
                value.Text(_) -> #("#4a4a4a", "#f2f2f2")
                value.TestPass -> #("#006400", "#e6ffe6")
                value.TestFail -> #("#b30000", "#ffe6e6")
                _ -> #("black", "white")
              }
          }

          let #(color, background_color) = case
            model.show_test_coverage,
            model.active_cell
          {
            False, None -> colors
            False, Some(active_cell) ->
              case grid.get(model.type_checked_grid, active_cell) {
                Error(_) -> colors
                Ok(typed_expr) ->
                  case typed_expr.type_ {
                    typ.TTestResult ->
                      case grid.get(model.value_grid, active_cell) {
                        Ok(value.TestPass) ->
                          case
                            squared_away_lang.dependency_list(
                              model.type_checked_grid,
                              typed_expr,
                              [],
                            )
                            |> list.contains(key)
                          {
                            False -> colors
                            True -> #("#006400", "#e6ffe6")
                          }
                        _ -> colors
                      }
                    _ -> colors
                  }
              }
            True, _ -> {
              case list.contains(deps, key) {
                True -> #("#006400", "#e6ffe6")
                False -> {
                  // If it's a formula not covered by a test, we'll make it orange 
                  case grid.get(model.type_checked_grid, key) {
                    Error(_) -> colors
                    Ok(te) ->
                      case grid.get(model.value_grid, key) {
                        Error(_) -> colors
                        Ok(v) ->
                          case v {
                            value.TestFail -> colors
                            value.TestPass -> colors
                            _ ->
                              case te {
                                typed_expr.BinaryOp(_, _, _, _)
                                | typed_expr.BuiltinSum(_, _)
                                | typed_expr.BuiltinAvg(_, _)
                                | typed_expr.Group(_, _)
                                | typed_expr.UnaryOp(_, _, _) -> #(
                                  "#FFA500",
                                  "#FFF8E1",
                                )

                                _ -> colors
                              }
                          }
                      }
                  }
                }
              }
            }
          }

          let col_width =
            dict.get(col_widths, key |> grid.col)
            |> result.unwrap(or: min_cell_size_ch)

          let input =
            html.input([
              on_input,
              on_focus,
              out_of_focus,
              value,
              on_keydown,
              id,
              attribute.type_("text"),
              error_class,
              attribute.style([
                #("background-color", background_color),
                #("color", color),
                #("text-align", alignment),
                #(
                  "width",
                  col_width
                  |> int.to_string
                    <> "ch",
                ),
              ]),
            ])

          html.td([attribute.style([#("border", "1px solid gray")])], [input])
        })

      html.tr([attribute.style([#("border", "1px solid gray")])], cells)
    })
    |> dict.to_list
    |> list.sort(fn(r1, r2) { int.compare(r1.0, r2.0) })
    |> list.map(fn(e) { e.1 })

  let grid =
    html.table(
      [
        attribute.style([
          #("height", "70vh"),
          #("width", "90vw"),
          #("overflow-y", "auto"),
          #("overflow-x", "auto"),
          #("display", "block"),
          #("border-collapse", "collapse"),
        ]),
      ],
      [html.tbody([], rows)],
    )

  let formula_mode_toggle =
    html.input([
      attribute.type_("checkbox"),
      attribute.id("formula_mode"),
      event.on_check(UserToggledFormulaMode),
    ])

  let formula_mode_toggle_label =
    html.label([attribute.for("formula_mode")], t("Show formulas"))

  let show_test_coverage_toggle =
    html.input([
      attribute.type_("checkbox"),
      attribute.id("test_coverage"),
      event.on_check(UserToggledShowTestCoverage),
    ])

  let show_test_coverage_toggle_label =
    html.label([attribute.for("test_coverage")], t("Show test coverage"))

  let save_button = html.button([event.on_click(UserClickedSaveBtn)], t("Save"))
  let load_button =
    html.input([
      attribute.type_("file"),
      attribute.id("csvupload"),
      event.on_input(UserUploadedFile),
    ])

  let #(passed, total) =
    model.value_grid
    |> grid.to_list
    |> list.map(pair.second)
    |> list.fold(#(0, 0), fn(acc, x) {
      let #(passed, total) = acc
      case x {
        Ok(value.TestPass) -> #(passed + 1, total + 1)
        Ok(value.TestFail) -> #(passed, total + 1)
        _ -> #(passed, total)
      }
    })

  let #(test_count_color, test_count_bg_color) = case passed == total {
    True -> #("#006400", "#e6ffe6")
    False -> #("#b30000", "#ffe6e6")
  }
  let test_count_html =
    html.label(
      [
        attribute.style([
          #("color", test_count_color),
          #("background-color", test_count_bg_color),
        ]),
      ],
      t(
        int.to_string(passed)
        <> "/"
        <> int.to_string(total)
        <> " tests passing.",
      ),
    )

  html.div([attribute.style([#("text-align", "center")])], [
    html.div([], [
      html.div([attribute.class("menu-item")], [
        formula_mode_toggle,
        formula_mode_toggle_label,
      ]),
      html.div([attribute.class("menu-item")], [
        show_test_coverage_toggle,
        show_test_coverage_toggle_label,
      ]),
      html.div([attribute.class("menu-item")], [load_button]),
      html.div([attribute.class("menu-item")], [save_button]),
      html.div([attribute.class("menu-item")], [test_count_html]),
    ]),
    grid,
    error_to_display,
  ])
}

fn error_view(re: renderable_error.RenderableError) {
  html.div(
    [
      attribute.style([
        #("background-color", "#ffe6e6"),
        #("color", "#b30000"),
        #("margin-top", "10px"),
        #("margin", "auto"),
        #("width", "90vw"),
        #("padding", "20px"),
        #("box-sizing", "border-box"),
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
