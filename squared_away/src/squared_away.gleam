import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/attribute.{class}
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import squared_away_lang as lang
import squared_away_lang/error
import squared_away_lang/interpreter/value

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
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let cols = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> string.to_graphemes
  let rows = list.range(1, 100)

  let src_grid =
    list.fold(cols, dict.new(), fn(grid, c) {
      list.fold(rows, dict.new(), fn(partial_grid, r) {
        let key = c <> int.to_string(r)
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
  Model(..model, value_grid:)
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
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> string.to_graphemes

  let rows =
    list.range(1, 100)
    |> list.map(int.to_string)
    |> list.map(fn(row) {
      let cells =
        list.map(alphabet, fn(col) {
          let key = col <> row
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let show_formula =
            model.active_cell == Some(key) || model.formula_mode
          let value =
            case show_formula {
              True -> assert_get(model.src_grid, key)
              False ->
                case assert_get(model.value_grid, key) {
                  Error(e) -> string.inspect(e)
                  Ok(v) -> value.value_to_string(v)
                }
            }
            |> attribute.value

          html.td([], {
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
  html.div([], [grid, formula_mode_toggle, formula_mode_toggle_label])
}

// We operate on a dict which we prefill with cell values on init,
// so we know the dict has value's for key's we're fetching
fn assert_get(d: dict.Dict(a, b), key: a) -> b {
  let assert Ok(v) = dict.get(d, key)
  v
}

fn t(input: String) {
  [html.text(input)]
}
