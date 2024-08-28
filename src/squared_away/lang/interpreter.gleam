import gleam/bool
import gleam/dict
import gleam/float
import gleam/int
import gleam/result
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

fn convert_grid(input: dict.Dict(String, Result(a, b))) -> dict.Dict(String, Result(a, Nil)) {
  use acc, key, val <- dict.fold(input, dict.new())
  dict.insert(acc, key, val |> result.nil_error)
}

pub fn interpret_grid(input: dict.Dict(String, Result(typechecker.TypedExpr, InterpretError))) -> dict.Dict(String, Result(Value, InterpretError)) {
  use acc, key, typed_expr <- dict.fold(input, dict.new())
  case typed_expr {
    Error(e) -> dict.insert(acc, key, Error(e))
    Ok(typed_expr) -> {
      let maybe_value = interpret(input, typed_expr)
      dict.insert(acc, key, maybe_value)
    }
  }
} 

pub fn typecheck_grid(
  input: dict.Dict(String, Result(parser.Expr, InterpretError)),
) -> dict.Dict(String, Result(typechecker.TypedExpr, InterpretError)) {
  use acc, key, expr <- dict.fold(input, dict.new())
  case expr {
    Error(e) -> dict.insert(acc, key, Error(e))
    Ok(expr) -> {
let maybe_typed_expr = typechecker.typecheck(convert_grid(input), expr) |> result.map_error(TypeError)
  dict.insert(acc, key, maybe_typed_expr)
    }
  }
  
}

pub fn parse_grid(
  input: dict.Dict(String, Result(List(scanner.Token), InterpretError)),
) -> dict.Dict(String, Result(parser.Expr, InterpretError)) {
  use acc, key, toks <- dict.fold(input, dict.new())
  case toks {
    Error(e) -> dict.insert(acc, key, Error(e))
    Ok(toks) -> {
      let expr = parser.parse(toks) |> result.map_error(ParseError)
      dict.insert(acc, key, expr)
    }
  }
}

pub fn scan_grid(
  input: dict.Dict(String, String),
) -> dict.Dict(String, Result(List(scanner.Token), InterpretError)) {
  use acc, key, src <- dict.fold(input, dict.new())
  let maybe_scanned = scanner.scan(src) |> result.map_error(ScanError)
  dict.insert(acc, key, maybe_scanned)
}

pub fn interpret(
  env: dict.Dict(String, Result(typechecker.TypedExpr, InterpretError)),
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
      case dict.get(env, cell_ref) {
        Ok(Error(e)) -> Error(e)
        Error(Nil) -> Error(RuntimeError("Uhhhh nil cell reference? I need to work out the semantics around Nil/Empty"))
        Ok(Ok(expr)) -> interpret(env, expr)
      }
    }
    typechecker.UnaryOp(_, op, expr) -> {
      use value <- result.try(interpret(env, expr))
      case op, value {
        parser.Negate, Integer(n) -> Ok(Integer(-n))
        parser.Negate, FloatingPointNumber(f) ->
          Ok(FloatingPointNumber(float.negate(f)))
        parser.Not, Boolean(b) -> Ok(Boolean(!b))
        _, _ ->
          panic as "These should be the only options if the typechecker is working"
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
