import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string
import lustre
import lustre/attribute
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
    active_cell: String,
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
      active_cell: "A1",
      src_grid:,
      value_grid: dict.new(),
    )
  let model = update_grid(model)
  #(model, effect.none())
}

type Msg {
  UserClickedCell(key: String)
  UserToggledFormulaMode(to: Bool)
  UserSetCellValue(key: String, val: String)
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
    UserClickedCell(key) -> {
      #(Model(..model, active_cell: key), effect.none())
    }
    UserToggledFormulaMode(formula_mode) -> #(
      Model(..model, formula_mode:),
      effect.none(),
    )
  }
}

fn view(model: Model) -> element.Element(Msg) {
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  let columns = [
    html.th([attribute.class("sticky-header")], [html.text("")]),
    ..list.map(string.to_graphemes(alphabet), fn(l) {
      html.th([attribute.class("sticky-header")], [html.text(l)])
    })
  ]

  let grid =
    html.div([attribute.class("table-container")], [
      html.table([], [
        html.thead([], [html.tr([], columns)]),
        html.tbody(
          [],
          list.repeat(Nil, 100)
            |> list.index_map(fn(_, i) {
              html.tr([], [
                html.th([attribute.class("sticky-column")], [
                  html.text(int.to_string(i + 1)),
                ]),
                ..list.map(string.to_graphemes(alphabet), fn(l) {
                  html.td([], {
                    let key = l <> int.to_string(i + 1)

                    [
                      html.input([
                        event.on_input(UserSetCellValue(key:, val: _)),
                        event.on_click(UserClickedCell(
                          l <> int.to_string(i + 1),
                        )),
                        attribute.value({
                          case model.active_cell == key || model.formula_mode {
                            True -> {
                              let assert Ok(v) = dict.get(model.src_grid, key)
                              v
                            }
                            False -> {
                              let assert Ok(v) = dict.get(model.value_grid, key)
                              case v {
                                Error(e) -> string.inspect(e)
                                Ok(v) -> value.value_to_string(v)
                              }
                            }
                          }
                        }),
                      ]),
                    ]
                  })
                })
              ])
            }),
        ),
      ]),
    ])

  // We filled the grid ourself so this shouldn't ever panic, if it does it's a 
  // bug and I'd rather find it sooner.
  let assert Ok(active_cell_value) =
    dict.get(model.value_grid, model.active_cell)

  html.div([], [
    html.div([], [grid]),
    html.p([], [
      html.text(model.active_cell <> ": " <> string.inspect(active_cell_value)),
    ]),
    html.input([
      attribute.type_("checkbox"),
      attribute.id("formula_mode"),
      event.on_check(UserToggledFormulaMode),
    ]),
    html.label([attribute.for("formula_mode")], [
      html.text("toggle formula mode"),
    ]),
  ])
}
