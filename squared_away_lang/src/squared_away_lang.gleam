import gleam/list
import gleam/option
import gleam/result
import squared_away_lang/error
import squared_away_lang/grid
import squared_away_lang/interpreter
import squared_away_lang/interpreter/value
import squared_away_lang/parser
import squared_away_lang/parser/expr
import squared_away_lang/scanner
import squared_away_lang/scanner/token
import squared_away_lang/typechecker
import squared_away_lang/typechecker/typed_expr

pub fn interpret_grid(
  input: grid.Grid(Result(typed_expr.TypedExpr, error.CompileError)),
) -> grid.Grid(Result(value.Value, error.CompileError)) {
  use _, typed_expr <- grid.map_values(input)
  case typed_expr {
    Error(e) -> Error(e)
    Ok(typed_expr) -> interpreter.interpret(input, typed_expr)
  }
}

pub fn typecheck_grid(
  input: grid.Grid(Result(expr.Expr, error.CompileError)),
) -> grid.Grid(Result(typed_expr.TypedExpr, error.CompileError)) {
  use _, expr <- grid.map_values(input)
  case expr {
    Error(e) -> Error(e)
    Ok(expr) -> typechecker.typecheck(input, expr)
  }
}

pub fn parse_grid(
  input: grid.Grid(Result(List(token.Token), error.CompileError)),
) -> grid.Grid(Result(expr.Expr, error.CompileError)) {
  use key, toks <- grid.map_values(input)
  case toks {
    Error(e) -> Error(e)
    Ok(toks) -> {
      // I need to enrich the builtin sum with it's key before parsing
      let toks =
        toks
        |> list.map(fn(t) {
          case t {
            token.BuiltinSum(option.None) -> token.BuiltinSum(option.Some(key))
            _ -> t
          }
        })

      let expr = parser.parse(toks)
      expr |> result.map_error(error.ParseError)
    }
  }
}

pub fn scan_grid(
  input: grid.Grid(String),
) -> grid.Grid(Result(List(token.Token), error.CompileError)) {
  use _, src <- grid.map_values(input)
  scanner.scan(src)
  |> result.map_error(error.ScanError)
}
