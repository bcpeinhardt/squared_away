import gleam/dict
import gleam/result
import gleam/string
import squared_away/lang/environment
import squared_away/lang/parser

pub type TypedExpr {
  Empty(type_: Typ)
  FloatLiteral(type_: Typ, f: Float)
  StringLiteral(type_: Typ, txt: String)
  IntegerLiteral(type_: Typ, n: Int)
  CellReference(type_: Typ, key: String)
  BooleanLiteral(type_: Typ, b: Bool)
  UnaryOp(type_: Typ, op: parser.UnaryOpKind, expr: TypedExpr)
  BinaryOp(type_: Typ, lhs: TypedExpr, op: parser.BinaryOpKind, rhs: TypedExpr)
  Group(type_: Typ, expr: TypedExpr)
}

pub type Typ {
  TNil
  TFloat
  TString
  TInt
  TBool
}

pub type TypeError {
  TypeError(context: String)
}

pub fn typecheck(
  env: environment.Environment,
  expr: parser.Expr,
) -> Result(TypedExpr, TypeError) {
  case expr {
    parser.Empty -> Ok(Empty(type_: TNil))
    parser.StringLiteral(txt) -> Ok(StringLiteral(type_: TString, txt:))
    parser.BooleanLiteral(b) -> Ok(BooleanLiteral(type_: TBool, b:))
    parser.FloatLiteral(f) -> Ok(FloatLiteral(type_: TFloat, f:))
    parser.IntegerLiteral(n) -> Ok(IntegerLiteral(type_: TInt, n:))
    parser.Group(inner) -> {
      use expr <- result.try(typecheck(env, inner))
      Ok(Group(type_: expr.type_, expr:))
    }
    parser.CellReference(key) -> {
      let ref_expr = dict.get(env, key) |> result.unwrap(or: parser.Empty)
      use expr <- result.try(typecheck(env, ref_expr))
      Ok(CellReference(type_: expr.type_, key:))
    }
    parser.UnaryOp(op, expr) -> {
      use expr <- result.try(typecheck(env, expr))
      case op, expr.type_ {
        parser.Negate, TInt | parser.Negate, TFloat ->
          Ok(UnaryOp(type_: expr.type_, op:, expr:))
        parser.Not, TBool -> Ok(UnaryOp(type_: expr.type_, op:, expr:))
        _, _ -> Error(TypeError("Unexpected type and operator combination"))
      }
    }
    parser.BinaryOp(lhs, op, rhs) -> {
      use lhs <- result.try(typecheck(env, lhs))
      use rhs <- result.try(typecheck(env, rhs))
      case lhs.type_, op, rhs.type_ {
        // Addition
        TFloat, parser.Add, TFloat ->
          Ok(BinaryOp(type_: TFloat, lhs:, op:, rhs:))
        TInt, parser.Add, TInt -> Ok(BinaryOp(type_: TInt, lhs:, op:, rhs:))

        // Subtraction 
        TFloat, parser.Subtract, TFloat ->
          Ok(BinaryOp(type_: TFloat, lhs:, op:, rhs:))
        TInt, parser.Subtract, TInt ->
          Ok(BinaryOp(type_: TInt, lhs:, op:, rhs:))

        // Multiplication
        TFloat, parser.Multiply, TFloat ->
          Ok(BinaryOp(type_: TFloat, lhs:, op:, rhs:))
        TInt, parser.Multiply, TInt ->
          Ok(BinaryOp(type_: TInt, lhs:, op:, rhs:))

        // Division
        TFloat, parser.Divide, TFloat ->
          Ok(BinaryOp(type_: TFloat, lhs:, op:, rhs:))
        TInt, parser.Divide, TInt -> Ok(BinaryOp(type_: TInt, lhs:, op:, rhs:))

        // Power
        TFloat, parser.Power, TFloat | TInt, parser.Power, TFloat ->
          Ok(BinaryOp(type_: TFloat, lhs:, op:, rhs:))

        // Equal and Not Equal Check 
        t1, parser.EqualCheck, t2 | t1, parser.NotEqualCheck, t2 if t1 == t2 ->
          Ok(BinaryOp(type_: TBool, lhs:, op:, rhs:))

        // Ordering Checks
        TFloat, parser.LessThanCheck, TFloat
        | TFloat, parser.LessThanOrEqualCheck, TFloat
        | TFloat, parser.GreaterThanOrEqualCheck, TFloat
        | TFloat, parser.GreaterThanCheck, TFloat
        | TInt, parser.LessThanCheck, TInt
        | TInt, parser.LessThanOrEqualCheck, TInt
        | TInt, parser.GreaterThanOrEqualCheck, TInt
        | TInt, parser.GreaterThanCheck, TInt
        | TString, parser.LessThanCheck, TString
        | TString, parser.LessThanOrEqualCheck, TString
        | TString, parser.GreaterThanOrEqualCheck, TString
        | TString, parser.GreaterThanCheck, TString
        -> Ok(BinaryOp(type_: TBool, lhs:, op:, rhs:))

        // Boolean Operations
        TBool, parser.And, TBool | TBool, parser.Or, TBool ->
          Ok(BinaryOp(type_: TBool, lhs:, op:, rhs:))

        _, _, _ ->
          Error(TypeError(
            "Unexpected arguments to binary operation: " <> string.inspect(op),
          ))
      }
    }
  }
}
