//// The way this is gonna work is we're going to start with
//// an initial set of grids for all the cells, and try to only
//// compile and update what we need based on the dependency graph.

import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/set
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

pub fn get_scanned(state: State) {
  state.cells
  |> grid.map_values(fn(_, cell) {
    result.map(cell.outcome, fn(c) { c.scanned })
  })
}

pub fn get_parsed(state: State) {
  state.cells
  |> grid.map_values(fn(_, cell) {
    result.map(cell.outcome, fn(c) { c.parsed })
  })
}

pub fn get_typechecked(state: State) {
  state.cells
  |> grid.map_values(fn(_, cell) {
    result.map(cell.outcome, fn(c) { c.typechecked })
  })
}

pub fn get_interpreted(state: State) {
  state.cells
  |> grid.map_values(fn(_, cell) {
    result.map(cell.outcome, fn(c) { c.interpreted })
  })
}

pub fn edit_cell(state: State, key: grid.GridKey, src: String) -> State {
  let old_cell = grid.get(state.cells, key)

  // Scan, parse, typecheck, and evaluate the cell
  let res = {
    use scanned <- result.try(
      scanner.scan(src) |> result.map_error(error.ScanError),
    )

    // Need to enrich some tokens with their key 
    let scanned =
      scanned
      |> list.map(fn(t) {
        case t {
          token.BuiltinSum(option.None) -> token.BuiltinSum(option.Some(key))
          token.BuiltinAvg(option.None) -> token.BuiltinAvg(option.Some(key))
          _ -> t
        }
      })

    use parsed <- result.try(
      parser.parse(scanned) |> result.map_error(error.ParseError),
    )

    use typechecked <- result.try(typechecker.typecheck(
      state |> get_parsed,
      parsed,
    ))

    // If the cell successfully typechecked, we can tell what cells
    // it depends on, so we need to update those entries in the dependency graph.

    let deps = dependency_list(state |> get_typechecked, typechecked, [])

    let deps_graph =
      deps
      |> list.fold(state.deps_graph, fn(s, dep) {
        dict.upsert(s, dep, fn(v) {
          case v {
            option.None -> [key]
            option.Some(lst) -> [key, ..lst]
          }
        })
      })

    use interpreted <- result.try(interpreter.interpret(
      state |> get_typechecked,
      typechecked,
    ))

    Ok(#(
      CompileSteps(scanned:, parsed:, typechecked:, interpreted:),
      deps_graph,
    ))
  }

  let state = case res {
    Error(e) -> {
      // If the process failed, we just set the error value as the cells value.
      State(
        ..state,
        cells: grid.insert(state.cells, key, Cell(src:, outcome: Error(e))),
      )
    }
    Ok(#(cell, deps_graph)) -> {
      State(
        cells: grid.insert(state.cells, key, Cell(src:, outcome: Ok(cell))),
        deps_graph:,
      )
    }
  }

  // Make sure to update all the cells dependents
  // We re-evaluate each dependent cell by calling "edit_cell" with
  // it's existing src code.
  let deps = state.deps_graph |> dict.get(key) |> result.unwrap(or: [])
  let new_state =
    deps
    |> list.fold(state, fn(s, k) {
      let c = grid.get(s.cells, k)
      edit_cell(s, k, c.src)
    })
  new_state
}

pub fn get_cell(state: State, key: grid.GridKey) -> Cell {
  grid.get(state.cells, key)
}

// TODO: Change this from a recursive function to a shallow function which only 
// returns the immediate dependencies and rely on the existing dependency graph for
// the rest
pub fn dependency_list(
  input: grid.Grid(Result(typed_expr.TypedExpr, error.CompileError)),
  te: typed_expr.TypedExpr,
  acc: List(grid.GridKey),
) -> List(grid.GridKey) {
  case te {
    typed_expr.BinaryOp(_, lhs:, op: _, rhs:) -> {
      let lhs = dependency_list(input, lhs, [])
      let rhs = dependency_list(input, rhs, [])
      let deps =
        set.union(set.from_list(lhs), set.from_list(rhs))
        |> set.to_list
      list.flatten([deps, acc])
    }
    typed_expr.BooleanLiteral(_, _) -> acc
    typed_expr.BuiltinSum(_, keys) | typed_expr.BuiltinAvg(_, keys) ->
      list.map(keys, fn(k) {
        case grid.get(input, k) {
          Error(_) -> [k]
          Ok(te) -> dependency_list(input, te, [k])
        }
      })
      |> list.flatten
      |> list.append(acc)
    typed_expr.CrossLabel(_, key, _, _) ->
      case grid.get(input, key) {
        Error(_) -> [key, ..acc]
        Ok(te) -> dependency_list(input, te, [key, ..acc])
      }
    typed_expr.Empty(_) -> acc
    typed_expr.FloatLiteral(_, _) -> acc
    typed_expr.Group(_, inner) -> dependency_list(input, inner, acc)
    typed_expr.IntegerLiteral(_, _) -> acc
    typed_expr.Label(_, key:, def_key:, txt: _) ->
      case grid.get(input, key) {
        Error(_) -> [key, def_key, ..acc]
        Ok(te) -> dependency_list(input, te, [key, def_key, ..acc])
      }
    typed_expr.LabelDef(_, _) -> acc
    typed_expr.PercentLiteral(_, _) -> acc
    typed_expr.UnaryOp(_, _, inner) -> dependency_list(input, inner, acc)
    typed_expr.UsdLiteral(_, _) -> acc
    typed_expr.StringLiteral(_, _) -> acc
  }
}
