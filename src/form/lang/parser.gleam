import form/lang/scanner
import gleam/result
import gleam/string

pub type FormExpr {
  Empty
  StringLiteral(txt: String)
  IntegerLiteral(n: Int)
  CellReference(key: String)
  BooleanLiteral(val: Bool)
  UnaryOp(op: UnaryOpKind, expr: FormExpr)
  BinaryOp(lhs: FormExpr, op: BinaryOpKind, rhs: FormExpr)
  Group(inner: FormExpr)
}

pub type BinaryOpKind {
  Add
  Subtract
  Multiply
  Divide
  Power
  EqualCheck
  NotEqualCheck
  LessThanCheck
  LessThanOrEqualCheck
  GreaterThanCheck
  GreaterThanOrEqualCheck
  And
  Or
}

pub type UnaryOpKind {
  Negate
  Not
}

pub type ParseError {
  ParseError(context: String)
}

pub fn parse(tokens: List(scanner.Token)) -> Result(FormExpr, ParseError) {
  case lhs_parse(tokens) {
    Error(e) -> Error(e)
    Ok(#(expr, [])) -> Ok(expr)
    Ok(#(expr, rest)) -> {
      use op_maker <- result.try(try_deep_binary_ops(rest))
      Ok(op_maker(expr))
    }
  }
}

fn lhs_parse(
  tokens: List(scanner.Token),
) -> Result(#(FormExpr, List(scanner.Token)), ParseError) {
  case tokens {
    [] -> Ok(#(Empty, []))
    // Let's do the single token patterns first
    [scanner.StringLiteral(str), ..rest] -> Ok(#(StringLiteral(str), rest))
    [scanner.IntegerLiteral(n), ..rest] -> Ok(#(IntegerLiteral(n), rest))
    [scanner.CellReference(key), ..rest] -> Ok(#(CellReference(key), rest))
    [scanner.TrueToken, ..rest] -> Ok(#(BooleanLiteral(True), rest))
    [scanner.FalseToken, ..rest] -> Ok(#(BooleanLiteral(False), rest))

    // Unary Ops
    [scanner.Minus, ..rest] -> {
      use #(parsed_remainder, rest) <- result.try(lhs_parse(rest))
      Ok(#(UnaryOp(Negate, parsed_remainder), rest))
    }
    [scanner.Bang, ..rest] -> {
      use #(parsed_remainder, rest) <- result.try(lhs_parse(rest))
      Ok(#(UnaryOp(Not, parsed_remainder), rest))
    }

    // Group
    [scanner.LParen, ..rest] -> {
      use #(body, rest) <- result.try(lhs_parse(rest))
      case rest {
        [scanner.RParen, ..rest] -> Ok(#(Group(body), rest))
        _ -> Error(ParseError("missing closing parentheses"))
      }
    }

    [x, ..] -> Error(ParseError("Unexpected token: " <> string.inspect(x)))
  }
}

fn try_deep_binary_ops(
  tokens: List(scanner.Token),
) -> Result(fn(FormExpr) -> FormExpr, ParseError) {
  case try_parse_binary_ops(tokens) {
    Error(e) -> Error(e)
    Ok(#(expr_maker, [])) -> Ok(expr_maker)
    Ok(#(expr_maker, rest)) -> {
      case try_deep_binary_ops(rest) {
        Error(e) -> Error(e)
        Ok(op_maker) -> {
          Ok(fn(lhs) { op_maker(expr_maker(lhs)) })
        }
      }
    }
  }
}

fn try_parse_binary_ops(
  tokens: List(scanner.Token),
) -> Result(#(fn(FormExpr) -> FormExpr, List(scanner.Token)), ParseError) {
  case tokens {
    [scanner.Plus, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, Add, rhs), rest))
    }
    [scanner.Minus, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, Subtract, rhs), rest))
    }
    [scanner.Star, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, Multiply, rhs), rest))
    }
    [scanner.Div, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, Divide, rhs), rest))
    }
    [scanner.StarStar, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, Power, rhs), rest))
    }
    [scanner.BangEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, NotEqualCheck, rhs), rest))
    }
    [scanner.EqualEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, EqualCheck, rhs), rest))
    }
    [scanner.LessEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, LessThanOrEqualCheck, rhs), rest))
    }
    [scanner.Less, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, LessThanCheck, rhs), rest))
    }
    [scanner.GreaterEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, GreaterThanOrEqualCheck, rhs), rest))
    }
    [scanner.Greater, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, GreaterThanCheck, rhs), rest))
    }
    [scanner.And, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, And, rhs), rest))
    }
    [scanner.Or, ..rest] -> {
      use #(rhs, rest) <- result.try(lhs_parse(rest))
      Ok(#(BinaryOp(_, Or, rhs), rest))
    }
    _ -> Error(ParseError("Not a binary operation"))
  }
}
