import squared_away/lang/interpreter
import squared_away/lang/parser
import squared_away/lang/scanner
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lustre
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

/// To start, our model will be a 5x5 grid of Strings
type Model {
  Model(
    active_cell: String,
    grid: Dict(String, parser.Expr),
    error: Option(interpreter.InterpretError),
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let cols = "ABCDE" |> string.to_graphemes
  let rows = [1, 2, 3, 4, 5]

  let grid =
    list.fold(cols, dict.new(), fn(grid, c) {
      list.fold(rows, dict.new(), fn(partial_grid, r) {
        let key = c <> int.to_string(r)
        partial_grid |> dict.insert(key, parser.Empty)
      })
      |> dict.merge(grid)
    })

  #(Model(active_cell: "A1", grid:, error: None), effect.none())
}

type Msg {
  UserClickedCell(key: String)
  UserSetCellValue(key: String, val: String)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserSetCellValue(key, val) -> {
      let res = {
        use tokens <- result.try(
          scanner.scan(val) |> result.map_error(interpreter.ScanError),
        )
        use expr <- result.try(
          parser.parse(tokens) |> result.map_error(interpreter.ParseError),
        )
        Ok(expr)
      }
      case res {
        Ok(val) -> #(
          Model(..model, grid: model.grid |> dict.insert(key, val)),
          effect.none(),
        )
        Error(e) -> #(Model(..model, error: Some(e)), effect.none())
      }
    }
    UserClickedCell(key) -> {
      #(Model(..model, active_cell: key), effect.none())
    }
  }
}

fn view(model: Model) -> element.Element(Msg) {
  let cells =
    dict.keys(model.grid)
    |> list.sort(string.compare)
    |> list.map(fn(c) {
      html.input([
        event.on_input(UserSetCellValue(key: c, val: _)),
        event.on_click(UserClickedCell(c)),
      ])
    })

  let active_cell_value = {
    let assert Ok(expr) = dict.get(model.grid, model.active_cell)
    string.inspect(interpreter.interpret(model.grid, expr))
  }

  html.div([], [
    html.div([], cells),
    html.p([], [html.text(model.active_cell <> ": " <> active_cell_value)]),
  ])
}
