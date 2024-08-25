import gleam/bool
import gleam/dict
import gleam/float
import gleam/int
import gleam/result
import gleam/string
import squared_away/lang/environment
import squared_away/lang/parser
import squared_away/lang/scanner
import squared_away/lang/typechecker

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
  TypeError(typechecker.TypeError)
  RuntimeError(String)
}

pub fn interpret(
  env: environment.Environment,
  expr: typechecker.TypedExpr,
) -> Result(Value, InterpretError) {
  case expr {
    typechecker.Empty(_) -> Ok(Empty)
    typechecker.Group(_, expr) -> interpret(env, expr)
    typechecker.StringLiteral(_, txt) -> Ok(Text(txt))
    typechecker.BooleanLiteral(_, b) -> Ok(Boolean(b))
    typechecker.IntegerLiteral(_, n) -> Ok(Integer(n))
    typechecker.FloatLiteral(_, f) -> Ok(FloatingPointNumber(f))
    typechecker.CellReference(_, cell_ref) -> {
      use cell_src <- result.try(
        dict.get(env, cell_ref)
        |> result.replace_error(RuntimeError("Referemced cell without data")),
      )
      use typed_expr <- result.try(
        typechecker.typecheck(env, cell_src) |> result.map_error(TypeError),
      )
      interpret(env, typed_expr)
    }
    typechecker.UnaryOp(_, op, expr) -> {
      use value <- result.try(interpret(env, expr))
      case op, value {
        parser.Negate, Integer(n) -> Ok(Integer(-n))
        parser.Negate, FloatingPointNumber(f) ->
          Ok(FloatingPointNumber(float.negate(f)))
        parser.Not, Boolean(b) -> Ok(Boolean(!b))
        _, _ ->
          panic as "These should be the only options if the typecher is working"
      }
    }
    typechecker.BinaryOp(_, lhs, op, rhs) -> {
      use lhs <- result.try(interpret(env, lhs))
      use rhs <- result.try(interpret(env, rhs))
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
        FloatingPointNumber(a), parser.Add, FloatingPointNumber(b) ->
          Ok(FloatingPointNumber(a +. b))
        FloatingPointNumber(a), parser.Subtract, FloatingPointNumber(b) ->
          Ok(FloatingPointNumber(a -. b))
        FloatingPointNumber(a), parser.Multiply, FloatingPointNumber(b) ->
          Ok(FloatingPointNumber(a *. b))
        FloatingPointNumber(a), parser.Divide, FloatingPointNumber(b) ->
          Ok(FloatingPointNumber(a /. b))
        FloatingPointNumber(a), parser.EqualCheck, FloatingPointNumber(b) ->
          Ok(Boolean(a == b))
        FloatingPointNumber(a), parser.NotEqualCheck, FloatingPointNumber(b) ->
          Ok(Boolean(a != b))
        FloatingPointNumber(a), parser.GreaterThanCheck, FloatingPointNumber(b) ->
          Ok(Boolean(a >. b))
        FloatingPointNumber(a),
          parser.GreaterThanOrEqualCheck,
          FloatingPointNumber(b)
        -> Ok(Boolean(a >=. b))
        FloatingPointNumber(a), parser.LessThanCheck, FloatingPointNumber(b) ->
          Ok(Boolean(a <. b))
        FloatingPointNumber(a),
          parser.LessThanOrEqualCheck,
          FloatingPointNumber(b)
        -> Ok(Boolean(a <=. b))

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

        _, _, _ ->
          panic as "these should be the only options if the typechecker is working properly"
      }
    }
  }
}
