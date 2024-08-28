import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import squared_away/lang/interpreter

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

/// To start, our model will be a 5x5 grid of Strings
type Model {
  Model(
    active_cell: String,
    src_grid: Dict(String, String),
    value_grid: Dict(
      String,
      Result(interpreter.Value, interpreter.InterpretError),
    ),
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let cols = "ABCDE" |> string.to_graphemes
  let rows = [1, 2, 3, 4, 5]

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

  let model = Model(active_cell: "A1", src_grid:, value_grid: dict.new())
  let model = update_grid(model)
  #(model, effect.none())
}

type Msg {
  UserClickedCell(key: String)
  UserSetCellValue(key: String, val: String)
}

fn update_grid(model: Model) -> Model {
  let scanned = interpreter.scan_grid(model.src_grid)
  let parsed = interpreter.parse_grid(scanned)
  let typechecked = interpreter.typecheck_grid(parsed)
  let value_grid = interpreter.interpret_grid(typechecked)
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
                  html.td([], [
                    html.input([
                      event.on_input(UserSetCellValue(
                        key: l <> int.to_string(i + 1),
                        val: _,
                      )),
                      event.on_click(UserClickedCell(l <> int.to_string(i + 1))),
                    ]),
                  ])
                })
              ])
            }),
        ),
      ]),
    ])

  let active_cell_value =
    dict.get(model.value_grid, model.active_cell)
    |> result.unwrap(or: Ok(interpreter.Empty))

  html.div([], [
    html.div([], [grid]),
    html.p([], [
      html.text(model.active_cell <> ": " <> string.inspect(active_cell_value)),
    ]),
  ])
}
