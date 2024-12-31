import gleam/option
import gleam/result
import squared_away/squared_away_lang/error
import squared_away/squared_away_lang/grid
import squared_away/squared_away_lang/interpreter/value
import squared_away/squared_away_lang/parser/expr
import squared_away/squared_away_lang/scanner/token
import squared_away/squared_away_lang/typechecker/typ
import squared_away/squared_away_lang/typechecker/typed_expr

pub type Outcome =
  Result(CompileSteps, error.CompileError)

/// The type describing the contents of a cell. 
pub type Cell {
  Cell(
    /// The source code in the cell.
    src: String,
    /// The compiled result of the cell.
    /// If the cell has not been compiled yet/needs recompiling, this field is set to None. 
    outcome: option.Option(Outcome),
  )
}

pub opaque type CompileSteps {
  CompileSteps(
    scanned: List(token.Token),
    parsed: expr.Expr,
    typechecked: typed_expr.TypedExpr,
    interpreted: value.Value,
  )
}

pub const empty_cell = Cell(
  src: "",
  outcome: option.Some(
    Ok(
      CompileSteps(
        scanned: [],
        parsed: expr.Empty,
        typechecked: typed_expr.Empty(type_: typ.TNil),
        interpreted: value.Empty,
      ),
    ),
  ),
)

/// Produces a new cell with some source code, which has not yet gone through
/// compilation.
pub fn new_uncompiled_cell(src: String) -> Cell {
  Cell(src:, outcome: option.None)
}

/// Wipes the compile information from the cell, so to be accessed it will
/// need to be recompiled.
pub fn invalidate(c: Cell) -> Cell {
  Cell(src: c.src, outcome: option.None)
}
