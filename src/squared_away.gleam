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
import squared_away/compiler
import squared_away/renderable_error
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/interpreter/value
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
    compiler_state: compiler.State,
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let model =
    Model(
      holding_shift: False,
      grid_width: initial_grid_width,
      grid_height: initial_grid_height,
      display_formulas: False,
      show_test_coverage: False,
      active_cell: None,
      compiler_state: compiler.init_state(
        initial_grid_width,
        initial_grid_height,
      ),
    )

  #(model, effect.none())
}

fn recalculate_col_width(model: Model, col: Int) -> Int {
  case model.display_formulas {
    False -> {
      // Based on value grid 
      model.compiler_state.cells
      |> grid.to_list
      |> list.filter_map(fn(c) {
        let #(k, c) = c
        case k |> grid.col == col {
          False -> Error(Nil)
          True ->
            case c.outcome {
              Error(_) -> Ok(c.src)
              Ok(cs) ->
                case model.active_cell == Some(k) {
                  False -> Ok(value.value_to_string(cs.interpreted))
                  True -> Ok(c.src)
                }
            }
        }
      })
      |> list.map(string.length)
    }
    True -> {
      // based on the src grid 
      model.compiler_state.cells
      |> grid.to_list
      |> list.filter_map(fn(c) {
        let #(k, c) = c
        case k |> grid.col == col {
          False -> Error(Nil)
          True -> Ok(string.length(c.src))
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
      let old = compiler.get_cell(model.compiler_state, key)

      // If updating a label, 
      // for every typed_expr that references the label, 
      // we need to update the source.
      // We do this by passing in the new label name to the typechecked expression
      // and re-serializing it to a string.
      let model =
        list.fold(model.compiler_state.cells |> grid.to_list, model, fn(acc, g) {
          let #(k, c) = g

          case c.outcome {
            // If it never compiled in the first place, don't try to change it.
            Error(_) -> acc
            Ok(cs) -> {
              // Create a new src string for the cell with the label updated
              let #(new_te, was_updated) =
                typed_expr.update_labels(cs.typechecked, old.src, val)

              // If the src for the cell changed, we need to update it
              case was_updated {
                False -> acc
                True ->
                  Model(
                    ..acc,
                    compiler_state: compiler.edit_cell(
                      model.compiler_state,
                      k,
                      typed_expr.to_string(new_te),
                    ),
                  )
              }
            }
          }
        })

      // Set the actual value in the cell.
      // The compiler will handle updating the cells dependencies.
      let model =
        Model(
          ..model,
          compiler_state: compiler.edit_cell(model.compiler_state, key, val),
        )

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
      set_active_cell_to(
        model,
        grid.cell_above(model.compiler_state.cells, cell),
      )
    UserPressedArrowLeft(cell) ->
      set_active_cell_to(
        model,
        grid.cell_to_the_left(model.compiler_state.cells, cell),
      )
    UserPressedArrowRight(cell) ->
      set_active_cell_to(
        model,
        grid.cell_to_the_right(model.compiler_state.cells, cell),
      )
    UserPressedArrowDown(cell) ->
      set_active_cell_to(
        model,
        grid.cell_underneath(model.compiler_state.cells, cell),
      )
    UserPressedEnter(cell) ->
      set_active_cell_to(
        model,
        grid.cell_underneath(model.compiler_state.cells, cell),
      )
    UserShiftPressedArrowRight(cell) -> {
      let maybe_cell_to_right =
        grid.cell_to_the_right(model.compiler_state.cells, cell)
      case maybe_cell_to_right {
        Error(Nil) -> #(model, effect.none())
        Ok(cell_to_right) -> {
          // Alright, this might be a slightly complex operation
          // If the current formula is a cross label, we want to 
          // produce the equivalent cross label but updated to the new
          // column value.

          let curr_cell = compiler.get_cell(model.compiler_state, cell)

          // if it's errored, don't copy it over
          use <- bool.guard(curr_cell.outcome |> result.is_error, #(
            model,
            effect.none(),
          ))
          // This assertion is safe because of the above bool.guard check
          let assert Ok(cs) = curr_cell.outcome

          let expr_with_labels_updated =
            typed_expr.visit_cross_labels(
              cs.typechecked,
              fn(key, row_label, col_label) {
                // For this case, we want to get the label directly to the right of the col label

                // This assertion is safe because we know the label is present in the grid,
                // because we got it from a typechecked cross_label
                let assert Ok(key_for_col) =
                  grid.find_first(model.compiler_state.cells, fn(c) {
                    c.src == col_label
                  })

                use key_for_new_col <- result.try(grid.cell_to_the_right(
                  model.compiler_state.cells,
                  key_for_col,
                ))

                let maybe_new_label =
                  compiler.get_cell(model.compiler_state, key_for_new_col)
                let get_new_label = fn(c: compiler.Cell) {
                  use l <- result.try(c.outcome |> result.nil_error)
                  case l.typechecked {
                    typed_expr.LabelDef(_, txt) -> Ok(txt)
                    _ -> Error(Nil)
                  }
                }
                use new_label <- result.try(get_new_label(maybe_new_label))

                // There was a cell to the right of the column label, so there 
                // must be a cell to the right of this one as well
                let assert Ok(new_key) =
                  grid.cell_to_the_right(model.compiler_state.cells, key)

                Ok(typed_expr.CrossLabel(
                  // The type here doesn't matter, I'm just producing a cross label I can
                  // serialize.
                  // This whole setup is pretty jank, I'd like to refactor it asap.
                  typ.TNil,
                  new_key,
                  row_label,
                  new_label,
                ))
              },
            )

          case expr_with_labels_updated {
            Error(_) -> #(model, effect.none())
            Ok(new_expr) -> {
              let id = grid.to_string(cell_to_right)
              let formula = typed_expr.to_string(new_expr)
              let new_model =
                Model(
                  ..model,
                  active_cell: Some(cell_to_right),
                  compiler_state: compiler.edit_cell(
                    model.compiler_state,
                    cell_to_right,
                    formula,
                  ),
                )
              #(new_model, focus(id))
            }
          }
        }
      }
    }
    UserShiftPressedArrowDown(cell) -> {
      let maybe_cell_below =
        grid.cell_underneath(model.compiler_state.cells, cell)
      case maybe_cell_below {
        Error(Nil) -> #(model, effect.none())
        Ok(cell_below) -> {
          let curr_cell = compiler.get_cell(model.compiler_state, cell)

          // if its an error, don't copy it over
          use <- bool.guard(curr_cell.outcome |> result.is_error, #(
            model,
            effect.none(),
          ))
          let assert Ok(cs) = curr_cell.outcome

          let expr_with_labels_updated =
            typed_expr.visit_cross_labels(
              cs.typechecked,
              fn(key, row_label, col_label) {
                // For this case, we want to get the label directly below the row label
                let assert Ok(key_for_row) =
                  grid.find_first(model.compiler_state.cells, fn(c) {
                    c.src == row_label
                  })

                use key_for_new_row <- result.try(grid.cell_underneath(
                  model.compiler_state.cells,
                  key_for_row,
                ))

                let maybe_new_label =
                  compiler.get_cell(model.compiler_state, key_for_new_row)
                let get_new_label = fn(c: compiler.Cell) {
                  use l <- result.try(c.outcome |> result.nil_error)
                  case l.typechecked {
                    typed_expr.LabelDef(_, txt) -> Ok(txt)
                    _ -> Error(Nil)
                  }
                }
                use new_label <- result.try(get_new_label(maybe_new_label))

                // We know there's a cell underneath because there was a cell underneath the row label
                let assert Ok(new_key) =
                  grid.cell_underneath(model.compiler_state.cells, key)

                Ok(typed_expr.CrossLabel(
                  // The type here doesn't matter, just need a cross_label to serialize.
                  // This needs refactoring for sure.
                  typ.TNil,
                  new_key,
                  new_label,
                  col_label,
                ))
              },
            )

          case expr_with_labels_updated {
            Error(_) -> #(model, effect.none())
            Ok(new_expr) -> {
              let formula = typed_expr.to_string(new_expr)
              let id = grid.to_string(cell_below)
              let new_model =
                Model(
                  ..model,
                  active_cell: Some(cell_below),
                  compiler_state: compiler.edit_cell(
                    model.compiler_state,
                    cell_below,
                    formula,
                  ),
                )
              #(new_model, focus(id))
            }
          }
        }
      }
    }
    UserClickedSaveBtn -> {
      let content =
        model.compiler_state.cells
        |> grid.map_values(fn(_, c) { c.src })
        |> grid.src_csv
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
      // Load in the src grid
      let src_grid =
        file_content
        |> grid.from_src_csv(initial_grid_width, initial_grid_height)

      // To create the new sheet, we'll start with an empty grid and fold
      // in each cell value one by one
      let new_grid =
        src_grid
        |> grid.fold(
          compiler.init_state(initial_grid_width, initial_grid_height),
          compiler.edit_cell,
        )

      #(
        Model(..model, compiler_state: new_grid, active_cell: None),
        effect.none(),
      )
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
  let error_to_display = case model.active_cell {
    None -> element.none()
    Some(k) ->
      case compiler.get_cell(model.compiler_state, k).outcome {
        Error(e) -> error_view(e |> error.to_renderable_error)
        Ok(_) -> element.none()
      }
  }

  let col_widths =
    list.range(1, initial_grid_width)
    |> list.map(fn(c) { #(c, recalculate_col_width(model, c)) })
    |> dict.from_list

  // In show test coverage mode, we need to check the dependency lists of *all*
  // passing tests

  let passing_test_cells =
    model.compiler_state.cells
    |> grid.to_list
    |> list.filter_map(fn(c) {
      let #(_, c) = c
      use cs <- result.try(c.outcome |> result.nil_error)
      case cs.interpreted {
        value.TestPass -> Ok(cs.typechecked)
        _ -> Error(Nil)
      }
    })

  let deps =
    passing_test_cells
    |> list.map(compiler.dependency_list(
      model.compiler_state |> compiler.get_typechecked,
      _,
      [],
    ))
    |> list.flatten
    |> list.unique

  let rows =
    model.compiler_state.cells.cells
    |> list.group(grid.row)
    |> dict.map_values(fn(_, keys) {
      let cells =
        list.sort(keys, fn(k1, k2) {
          int.compare(k1 |> grid.col(), k2 |> grid.col())
        })
        |> list.map(fn(key) {
          let cell = compiler.get_cell(model.compiler_state, key)
          let on_keydown = event.on("keydown", key_press_event(_, key))
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let id = attribute.id(grid.to_string(key))
          let value =
            case model.display_formulas, model.active_cell == Some(key) {
              True, _ | False, True -> cell.src
              False, _ ->
                case cell.outcome {
                  Error(_) -> cell.src
                  Ok(cs) -> cs.interpreted |> value.value_to_string
                }
            }
            |> attribute.value

          let alignment = case
            model.active_cell == Some(key) || model.display_formulas
          {
            True ->
              case cell.outcome {
                Error(_) -> "left"
                Ok(cs) ->
                  case cs.typechecked {
                    typed_expr.PercentLiteral(_, _)
                    | typed_expr.BooleanLiteral(_, _)
                    | typed_expr.UsdLiteral(_, _)
                    | typed_expr.IntegerLiteral(_, _)
                    | typed_expr.FloatLiteral(_, _) -> "right"
                    _ -> "left"
                  }
              }
            False ->
              case cell.outcome {
                Error(_) -> "left"
                Ok(cs) ->
                  case cs.interpreted {
                    value.Percent(_)
                    | value.Integer(_)
                    | value.FloatingPointNumber(_)
                    | value.Usd(_)
                    | value.Boolean(_) -> "right"
                    value.TestFail | value.TestPass | value.Empty -> "center"
                    value.Text(_) -> "left"
                    value.DoNotEvaluate(_) -> "left"
                  }
              }
          }

          let error_class = case cell.outcome |> result.is_error {
            False -> attribute.none()
            True -> attribute.class("errorcell")
          }

          let colors = case cell.outcome {
            Error(_) -> #("#b30000", "#ffe6e6")
            Ok(cs) ->
              case cs.interpreted {
                value.DoNotEvaluate(_) -> #("#4a4a4a", "#f2f2f2")
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
              case
                compiler.get_cell(model.compiler_state, active_cell).outcome
              {
                Error(_) -> colors
                Ok(cs) ->
                  case cs.interpreted {
                    value.TestPass ->
                      case
                        model.compiler_state.deps_graph
                        |> dict.filter(fn(_, v) {
                          list.contains(v, active_cell)
                        })
                        |> dict.keys
                        |> list.contains(key)
                      {
                        False -> colors
                        True -> #("#006400", "#e6ffe6")
                      }
                    _ -> colors
                  }
              }
            True, _ -> {
              case list.contains(deps, key) {
                True -> #("#006400", "#e6ffe6")
                False -> {
                  // If it's a formula not covered by a test, we'll make it orange 
                  case cell.outcome {
                    Error(_) -> colors
                    Ok(cs) ->
                      case cs.interpreted {
                        value.TestFail -> colors
                        value.TestPass -> colors
                        _ ->
                          case cs.typechecked {
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
    model.compiler_state.cells
    |> grid.to_list
    |> list.map(pair.second)
    |> list.fold(#(0, 0), fn(acc, x) {
      let #(passed, total) = acc
      case x.outcome {
        Error(_) -> acc
        Ok(cs) ->
          case cs.interpreted {
            value.TestFail -> #(passed, total + 1)
            value.TestPass -> #(passed + 1, total + 1)
            _ -> acc
          }
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
