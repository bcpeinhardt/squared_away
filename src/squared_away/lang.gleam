import gleam/dict
import gleam/result
import squared_away/lang/error
import squared_away/lang/interpreter
import squared_away/lang/interpreter/value
import squared_away/lang/parser
import squared_away/lang/parser/expr
import squared_away/lang/scanner
import squared_away/lang/scanner/token
import squared_away/lang/typechecker
import squared_away/lang/typechecker/typed_expr

pub fn interpret_grid(
  input: dict.Dict(String, Result(typed_expr.TypedExpr, error.CompileError)),
) -> dict.Dict(String, Result(value.Value, error.CompileError)) {
  use acc, key, typed_expr <- dict.fold(input, dict.new())
  case typed_expr {
    Error(e) -> dict.insert(acc, key, Error(e))
    Ok(typed_expr) -> {
      let maybe_value = interpreter.interpret(input, typed_expr)
      dict.insert(acc, key, maybe_value)
    }
  }
}

pub fn typecheck_grid(
  input: dict.Dict(String, Result(expr.Expr, error.CompileError)),
) -> dict.Dict(String, Result(typed_expr.TypedExpr, error.CompileError)) {
  use acc, key, expr <- dict.fold(input, dict.new())
  case expr {
    Error(e) -> dict.insert(acc, key, Error(e))
    Ok(expr) -> {
      let maybe_typed_expr = typechecker.typecheck(input, expr)
      dict.insert(acc, key, maybe_typed_expr)
    }
  }
}

pub fn parse_grid(
  input: dict.Dict(String, Result(List(token.Token), error.CompileError)),
) -> dict.Dict(String, Result(expr.Expr, error.CompileError)) {
  use acc, key, toks <- dict.fold(input, dict.new())
  case toks {
    Error(e) -> dict.insert(acc, key, Error(e))
    Ok(toks) -> {
      let expr = parser.parse(toks)
      dict.insert(acc, key, expr |> result.map_error(error.ParseError))
    }
  }
}

pub fn scan_grid(
  input: dict.Dict(String, String),
) -> dict.Dict(String, Result(List(token.Token), error.CompileError)) {
  use acc, key, src <- dict.fold(input, dict.new())
  let maybe_scanned = scanner.scan(src) |> result.map_error(error.ScanError)
  dict.insert(acc, key, maybe_scanned)
}
