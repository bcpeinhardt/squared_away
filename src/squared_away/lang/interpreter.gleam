import gleam/float
import squared_away/lang/parser
import squared_away/lang/scanner
import gleam/bool
import gleam/dict
import gleam/int
import gleam/result
import gleam/string

/// Our very basic starting environment
pub type Environment =
  dict.Dict(String, parser.Expr)

pub type Value {
  Empty
  Text(inner: String)
  Integer(n: Int)
  FloatingPointNumber(f: Float)
  Boolean(b: Bool)
}

pub fn value_to_string(fv: Value) -> String {
  case fv {
    Empty -> ""
    Text(t) -> t
    Integer(n) -> int.to_string(n)
    Boolean(b) -> bool.to_string(b)
    FloatingPointNumber(f) -> float.to_string(f)
  }
}

pub type InterpretError {
  ScanError(scanner.ScanError)
  ParseError(parser.ParseError)
  RuntimeError(String)
}

pub fn interpret(
  environment: Environment,
  expr: parser.Expr,
) -> Result(Value, InterpretError) {
  case expr {
    parser.Empty -> Ok(Empty)
    parser.Group(expr) -> interpret(environment, expr)
    parser.StringLiteral(txt) -> Ok(Text(txt))
    parser.BooleanLiteral(b) -> Ok(Boolean(b))
    parser.IntegerLiteral(n) -> Ok(Integer(n))
    parser.FloatLiteral(f) -> Ok(FloatingPointNumber(f))
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
          
        // Float operations
        FloatingPointNumber(a), parser.Add, FloatingPointNumber(b) -> Ok(FloatingPointNumber(a +. b))
        FloatingPointNumber(a), parser.Subtract, FloatingPointNumber(b) -> Ok(FloatingPointNumber(a -. b))
        FloatingPointNumber(a), parser.Multiply, FloatingPointNumber(b) -> Ok(FloatingPointNumber(a *. b))
        FloatingPointNumber(a), parser.Divide, FloatingPointNumber(b) -> Ok(FloatingPointNumber(a /. b))
        FloatingPointNumber(a), parser.EqualCheck, FloatingPointNumber(b) -> Ok(Boolean(a == b))
        FloatingPointNumber(a), parser.NotEqualCheck, FloatingPointNumber(b) -> Ok(Boolean(a != b))
        FloatingPointNumber(a), parser.GreaterThanCheck, FloatingPointNumber(b) -> Ok(Boolean(a >. b))
        FloatingPointNumber(a), parser.GreaterThanOrEqualCheck, FloatingPointNumber(b) ->
          Ok(Boolean(a >=. b))
        FloatingPointNumber(a), parser.LessThanCheck, FloatingPointNumber(b) -> Ok(Boolean(a <. b))
        FloatingPointNumber(a), parser.LessThanOrEqualCheck, FloatingPointNumber(b) ->
          Ok(Boolean(a <=. b))
          
        // Exponents
        Integer(a), parser.Power, FloatingPointNumber(b) -> {
          let assert Ok(p) = int.power(a, b)
          Ok(FloatingPointNumber(p))
        }
        FloatingPointNumber(a), parser.Power, FloatingPointNumber(b) -> {
          let assert Ok(p) = float.power(a, b)
          Ok(FloatingPointNumber(p))
        }

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
