import gleam/bool
import gleam/int
import form/lang/parser
import form/lang/scanner
import gleam/dict
import gleam/result
import gleam/string

/// Our very basic starting environment
pub type Environment =
  dict.Dict(String, parser.FormExpr)

pub type FormValue {
  Empty
  Text(inner: String)
  Integer(n: Int)
  Boolean(b: Bool)
}

pub fn form_value_to_string(fv: FormValue) -> String {
  case fv {
    Empty -> ""
    Text(t) -> t 
    Integer(n) -> int.to_string(n)
    Boolean(b) -> bool.to_string(b)
  }
}

pub type InterpretError {
  ScanError(scanner.ScanError)
  ParseError(parser.ParseError)
  RuntimeError(String)
}

pub fn interpret(
  environment: Environment,
  expr: parser.FormExpr,
) -> Result(FormValue, InterpretError) {
  case expr {
    parser.Empty -> Ok(Empty)
    parser.Group(expr) -> interpret(environment, expr)
    parser.StringLiteral(txt) -> Ok(Text(txt))
    parser.BooleanLiteral(b) -> Ok(Boolean(b))
    parser.IntegerLiteral(n) -> Ok(Integer(n))
    parser.CellReference(cell_ref) -> {
      use cell_src <- result.try(
        dict.get(environment, cell_ref)
        |> result.replace_error(RuntimeError("Referemced cell without data")),
      )
      interpret(environment, cell_src)
    }
    parser.UnaryOp(op, expr) -> {
      use value <- result.try(interpret(environment, expr))
      case op, value {
        parser.Negate, Integer(n) -> Ok(Integer(-n))
        parser.Not, Boolean(b) -> Ok(Boolean(!b))
        _, _ ->
          Error(RuntimeError(
            "Unexpected unary operation: "
            <> string.inspect(op)
            <> " not compatible with "
            <> string.inspect(value),
          ))
      }
    }
    parser.BinaryOp(lhs, op, rhs) -> {
      use lhs <- result.try(interpret(environment, lhs))
      use rhs <- result.try(interpret(environment, rhs))
      case lhs, op, rhs {
        // Integer operations
        Integer(a), parser.Add, Integer(b) -> Ok(Integer(a + b))
        Integer(a), parser.Subtract, Integer(b) -> Ok(Integer(a - b))
        Integer(a), parser.Multiply, Integer(b) -> Ok(Integer(a * b))
        Integer(a), parser.Divide, Integer(b) -> Ok(Integer(a / b))
        Integer(a), parser.EqualCheck, Integer(b) -> Ok(Boolean(a == b))
        Integer(a), parser.NotEqualCheck, Integer(b) -> Ok(Boolean(a != b))
        Integer(a), parser.GreaterThanCheck, Integer(b) -> Ok(Boolean(a > b))
        Integer(a), parser.GreaterThanOrEqualCheck, Integer(b) ->
          Ok(Boolean(a >= b))
        Integer(a), parser.LessThanCheck, Integer(b) -> Ok(Boolean(a < b))
        Integer(a), parser.LessThanOrEqualCheck, Integer(b) ->
          Ok(Boolean(a <= b))

        // Boolean operations
        Boolean(a), parser.And, Boolean(b) -> Ok(Boolean(a && b))
        Boolean(a), parser.Or, Boolean(b) -> Ok(Boolean(a || b))
        Boolean(a), parser.EqualCheck, Boolean(b) -> Ok(Boolean(a == b))
        Boolean(a), parser.NotEqualCheck, Boolean(b) -> Ok(Boolean(a != b))

        _, _, _ -> Error(RuntimeError("Unexpected binary operation"))
      }
    }
  }
}
