//// The way this is gonna work is we're going to start with 
//// an initial set of grids for all the cells, and try to only
//// compile and update what we need based on the dependency graph.

import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/typechecker/typed_expr
import squared_away/squared_away_lang/interpreter/value
import squared_away/squared_away_lang/scanner/token

pub type State {
  State(
    src: grid.Grid(String),
    scanned: grid.Grid(Result(List(token.Token), error.CompileError)),
    parsed: grid.Grid(Result(expr.Expr, error.CompileError)),
    typechecked: grid.Grid(Result(typed_expr.TypedExpr, error.CompileError)),
    interpreted: grid.Grid(Result(value.Value, error.CompileError))
  )
}

// Create an "empty" grid with all the appropriate values.
// Cheating like this will let us build the whole pipeline as
// though we've already calculated a valid dependency list.
pub fn init_state(width: Int, height: Int) -> State {
  let src = grid.new(width, height, "")
  let scanned = grid.new(width, height, Ok([]))
  let parsed = grid.new(width, height, Ok(expr.Empty))
  let typechecked = grid.new(width, height, Ok(typed_expr.Empty(typ.TNil)))
  let interpreted = grid.new(width, height, Ok(value.Empty))
  
  State(
    src:, scanned:, parsed:, typechecked:, interpreted:
  )
}