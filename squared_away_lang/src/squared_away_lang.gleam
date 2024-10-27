import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
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
import squared_away_lang/typechecker/typ
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
  use key, expr <- grid.map_values(input)
  case expr {
    Error(e) -> Error(e)
    Ok(expr.Label(txt)) -> {
      case grid.cell_to_the_right(input, key) {
        Error(Nil) -> Ok(typed_expr.Label(type_: typ.TNil, txt:))
        Ok(new_key) -> {
          let val = grid.get(input, new_key)
          case val {
            Error(e) -> Error(e)
            Ok(val) -> {
              case typechecker.typecheck(input, val) {
                Error(e) -> Error(e)
                Ok(typed_val) ->
                  Ok(typed_expr.Label(type_: typed_val.type_, txt:))
              }
            }
          }
        }
      }
    }
    Ok(expr) -> typechecker.typecheck(input, expr)
  }
}

pub fn parse_grid(
  input: grid.Grid(Result(List(token.Token), error.CompileError)),
) -> grid.Grid(Result(expr.Expr, error.CompileError)) {
  use _, toks <- grid.map_values(input)
  case toks {
    Error(e) -> Error(e)
    Ok(toks) -> {
      let expr = parser.parse(toks)
      expr |> result.map_error(error.ParseError)
    }
  }
}

pub fn scan_grid(
  input: grid.Grid(String),
) -> grid.Grid(Result(List(token.Token), error.CompileError)) {
  use _, src <- grid.map_values(input)
  let maybe_scanned =
    scanner.scan(src)
    |> result.map_error(error.ScanError)
    |> result.map(list.map(_, fn(t) {
      case t {
        token.LabelDef(txt) -> token.LabelDef(txt)
        _ -> t
      }
    }))
  maybe_scanned
}
