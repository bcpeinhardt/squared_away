import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lustre
import lustre/attribute.{class}
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import pprint
import squared_away_lang as lang
import squared_away_lang/error
import squared_away_lang/interpreter/value
import squared_away_lang/typechecker/typ
import squared_away_lang/typechecker/type_error

const grid_width = 5

const grid_height = 5

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

/// To start, our model will be a 5x5 grid of Strings
type Model {
  Model(
    formula_mode: Bool,
    active_cell: Option(String),
    src_grid: Dict(String, String),
    value_grid: Dict(String, Result(value.Value, error.CompileError)),
    errors_to_display: List(#(String, error.CompileError)),
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let cols = list.range(1, grid_width)
  let rows = list.range(1, grid_height)

  let src_grid =
    list.fold(cols, dict.new(), fn(grid, c) {
      list.fold(rows, dict.new(), fn(partial_grid, r) {
        let key = int.to_string(c) <> "_" <> int.to_string(r)
        partial_grid |> dict.insert(key, "")
      })
      |> dict.merge(grid)
    })

  // We could presume that our value_grid starts with all empty,
  // but instead I think we should scan, parse, typecheck, and 
  // interpret the grid on init, as later we'll pass in saved files

  let model =
    Model(
      formula_mode: False,
      active_cell: None,
      src_grid:,
      value_grid: dict.new(),
      errors_to_display: [],
    )
    |> update_grid

  #(model, effect.none())
}

type Msg {
  UserToggledFormulaMode(to: Bool)
  UserSetCellValue(key: String, val: String)
  UserFocusedOnCell(key: String)
  UserFocusedOffCell
}

fn update_grid(model: Model) -> Model {
  let scanned = lang.scan_grid(model.src_grid)
  let parsed = lang.parse_grid(scanned)
  let typechecked = lang.typecheck_grid(parsed)
  let value_grid = lang.interpret_grid(typechecked)

  // Loop over the grid to see if there's any errors to display
  let errors_to_display =
    dict.fold(value_grid, [], fn(acc, key, val) {
      case val {
        Error(err) -> [#(key, err), ..acc]
        Ok(_) -> acc
      }
    })

  Model(..model, value_grid:, errors_to_display:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserSetCellValue(key, val) -> {
      let model =
        Model(..model, src_grid: dict.insert(model.src_grid, key, val))
      #(update_grid(model), effect.none())
    }
    UserToggledFormulaMode(formula_mode) -> #(
      Model(..model, formula_mode:),
      effect.none(),
    )
    UserFocusedOnCell(key) -> {
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
    UserFocusedOffCell -> {
      #(Model(..model, active_cell: None), effect.none())
    }
  }
}

fn view(model: Model) -> element.Element(Msg) {
  let columns = list.range(1, grid_width) |> list.map(int.to_string)

  let error_to_display =
    list.find_map(model.errors_to_display, fn(e) {
      case Some(e.0) == model.active_cell {
        False -> Error(Nil)
        True -> Ok(error_view(e.1))
      }
    })
    |> result.unwrap(or: html.div([], []))

  let rows =
    list.range(1, grid_height)
    |> list.map(int.to_string)
    |> list.map(fn(row) {
      let cells =
        list.map(columns, fn(col) {
          let key = col <> "_" <> row
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let show_formula =
            model.active_cell == Some(key) || model.formula_mode
          let value =
            case show_formula {
              True ->
                case dict.get(model.src_grid, key) {
                  Error(_) -> no_value_found_txt()
                  Ok(src) -> src
                }
              False ->
                case dict.get(model.value_grid, key) {
                  Error(_) -> no_value_found_txt()
                  Ok(v) -> {
                    case v {
                      Error(e) -> error.error_type_string(e)
                      Ok(v) -> value.value_to_string(v)
                    }
                  }
                }
            }
            |> attribute.value

          let styles = case
            list.find(model.errors_to_display, fn(i) { i.0 == key })
          {
            Error(Nil) -> []
            Ok(_) -> [attribute.style([#("background-color", "red")])]
          }

          html.td(styles, {
            [html.input([on_input, on_focus, out_of_focus, value])]
          })
        })

      html.tr([], cells)
    })

  let grid =
    html.div([class("table-container")], [
      html.table([], [html.tbody([], rows)]),
    ])

  let formula_mode_toggle =
    html.input([
      attribute.type_("checkbox"),
      attribute.id("formula_mode"),
      event.on_check(UserToggledFormulaMode),
    ])

  let formula_mode_toggle_label =
    html.label([attribute.for("formula_mode")], t("toggle formula mode"))

  html.div([], [
    formula_mode_toggle,
    formula_mode_toggle_label,
    error_to_display,
    grid,
  ])
}

fn no_value_found_txt() {
  "No value found"
}

fn error_view(e: error.CompileError) {
  case e {
    error.TypeError(te) -> {
      case te {
        type_error.IncorrectTypesForBinaryOp(lhs, rhs, bo) ->
          html.div([], [
            html.h4(
              [],
              t(
                "Type Error: Incorrect types for binary operation "
                <> type_error.describe_binary_op_kind_for_err(bo),
              ),
            ),
            html.p(
              [],
              t(
                "The `&&` operator is specifically for booleans values (TRUE/FALSE). ",
              ),
            ),
            html.p(
              [],
              t(
                "You provided a "
                <> typ.to_string(lhs)
                <> " on the left and a "
                <> typ.to_string(rhs)
                <> " on the right",
              ),
            ),
            html.p(
              [],
              t(
                "Hint: If you're trying to verify both values have been provided, use the global `notempty` function like so: notempty(myvar) && notempty(myothervar).",
              ),
            ),
          ])
        type_error.TypeError(txt) -> html.div([], t(txt))
      }
    }
    _ -> html.p([], t(error.to_string(e)))
  }
}

fn t(input: String) {
  [html.text(input)]
}
