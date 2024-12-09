//// The way this is gonna work is we're going to start with
//// an initial set of grids for all the cells, and try to only
//// compile and update what we need based on the dependency graph.

import gleam/option
import gleam/dict
import gleam/list
import gleam/result
import squared_away/squared_away_lang
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/interpreter
import squared_away/squared_away_lang/interpreter/value
import squared_away/squared_away_lang/parser
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/scanner
import squared_away/squared_away_lang/scanner/token
import squared_away/squared_away_lang/typechecker
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/typechecker/typed_expr

pub type Cell {
  Cell(src: String, outcome: Result(CompileSteps, error.CompileError))
}

pub type CompileSteps {
  CompileSteps(
    scanned: List(token.Token),
    parsed: expr.Expr,
    typechecked: typed_expr.TypedExpr,
    interpreted: value.Value,
  )
}

const empty_cell = Cell(
  src: "",
  outcome: Ok(
    CompileSteps(
      scanned: [],
      parsed: expr.Empty,
      typechecked: typed_expr.Empty(type_: typ.TNil),
      interpreted: value.Empty,
    ),
  ),
)

pub type State {
  State(
    cells: grid.Grid(Cell),
    deps_graph: dict.Dict(grid.GridKey, List(grid.GridKey)),
  )
}

// Create an "empty" grid with all the appropriate values.
// Cheating like this will let us build the whole pipeline as
// though we've already calculated a valid dependency list.
pub fn init_state(width: Int, height: Int) -> State {
  State(cells: grid.new(width, height, empty_cell), deps_graph: dict.new())
}

fn get_parsed(state: State) -> grid.Grid(Result(expr.Expr, error.CompileError)) {
  state.cells
  |> grid.map_values(fn(_, cell) {
    result.map(cell.outcome, fn(c) { c.parsed })
  })
}

fn get_typechecked(state: State) {
  state.cells
  |> grid.map_values(fn(_, cell) {
    result.map(cell.outcome, fn(c) { c.typechecked })
  })
}

pub fn edit_cell(state: State, key: grid.GridKey, src: String) -> State {
  // Scan, parse, typecheck, and evaluate the cell
  let res = {
    use scanned <- result.try(
      scanner.scan(src) |> result.map_error(error.ScanError),
    )
    use parsed <- result.try(
      parser.parse(scanned) |> result.map_error(error.ParseError),
    )
    use typechecked <- result.try(typechecker.typecheck(
      state |> get_parsed,
      parsed,
    ))

    // If the cell successfully typechecked, we can tell what cells
    // it depends on, so we need to update those entries in the dependency graph.

    let deps =
      squared_away_lang.dependency_list(
        state |> get_typechecked,
        typechecked,
        [],
      )
    let deps_graph =
      deps
      |> list.fold(state.deps_graph, fn(s, dep) {
        dict.upsert(
          s,
          dep,
          fn(v) {
            case v {
              option.None -> [key]
              option.Some(lst) -> [key, ..lst]
            }
          },
        )
      })

    use interpreted <- result.try(interpreter.interpret(
      state |> get_typechecked,
      typechecked,
    ))
    Ok(CompileSteps(scanned:, parsed:, typechecked:, interpreted:))
  }

  // Make sure to update all the cells dependents
  // We re-evaluate each dependent cell by calling "edit_cell" with
  // it's existing src code.
  let deps = state.deps_graph |> dict.get(key) |> result.unwrap(or: [])
  let state =
    deps
    |> list.fold(state, fn(s, k) { edit_cell(s, k, grid.get(s.cells, k).src) })

  State(..state, cells: grid.insert(state.cells, key, Cell(src:, outcome: res)))
}
