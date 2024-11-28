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
import squared_away/squared_away_lang/typechecker/typed_expr

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
        set.symmetric_difference(set.from_list(lhs), set.from_list(rhs))
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
    typed_expr.Label(_, key:, txt: _) ->
      case grid.get(input, key) {
        Error(_) -> [key, ..acc]
        Ok(te) -> dependency_list(input, te, [key, ..acc])
      }
    typed_expr.LabelDef(_, _) -> acc
    typed_expr.PercentLiteral(_, _) -> acc
    typed_expr.UnaryOp(_, _, inner) -> dependency_list(input, inner, acc)
    typed_expr.UsdLiteral(_, _) -> acc
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
            token.BuiltinAvg(option.None) -> token.BuiltinAvg(option.Some(key))
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
