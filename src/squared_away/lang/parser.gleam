import gleam/dict
import gleam/result
import gleam/string
import squared_away/lang/scanner

pub type Expr {
  Empty
  FloatLiteral(f: Float)
  StringLiteral(txt: String)
  IntegerLiteral(n: Int)
  CellReference(key: String)
  BooleanLiteral(val: Bool)
  UnaryOp(op: UnaryOpKind, expr: Expr)
  BinaryOp(lhs: Expr, op: BinaryOpKind, rhs: Expr)
  Group(inner: Expr)
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

pub fn parse(tokens: List(scanner.Token)) -> Result(Expr, ParseError) {
  use #(expr, rest) <- result.try(do_parse(tokens))
  case rest {
    [] -> Ok(expr)
    _ -> Error(ParseError("After parsing there were leftover tokens"))
  }
}

fn do_parse(
  tokens: List(scanner.Token),
) -> Result(#(Expr, List(scanner.Token)), ParseError) {
  case tokens {
    [] -> Ok(#(Empty, []))
    // Let's do the single token patterns first
    [scanner.StringLiteral(str), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(StringLiteral(str)), rest))
        Error(_) -> Ok(#(StringLiteral(str), rest))
      }
    }
    [scanner.IntegerLiteral(n), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(IntegerLiteral(n)), rest))
        Error(_) -> Ok(#(IntegerLiteral(n), rest))
      }
    }
    [scanner.FloatLiteral(f), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(FloatLiteral(f)), rest))
        Error(_) -> Ok(#(FloatLiteral(f), rest))
      }
    }
    [scanner.CellReference(key), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(CellReference(key)), rest))
        Error(_) -> Ok(#(CellReference(key), rest))
      }
    }
    [scanner.TrueToken, ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(BooleanLiteral(True)), rest))
        Error(_) -> Ok(#(BooleanLiteral(True), rest))
      }
    }
    [scanner.FalseToken, ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(BooleanLiteral(False)), rest))
        Error(_) -> Ok(#(BooleanLiteral(False), rest))
      }
    }

    // Unary Ops
    [scanner.Minus, ..rest] -> {
      use #(parsed_remainder, rest) <- result.try(do_parse(rest))
      Ok(#(UnaryOp(Negate, parsed_remainder), rest))
    }
    [scanner.Bang, ..rest] -> {
      use #(parsed_remainder, rest) <- result.try(do_parse(rest))
      Ok(#(UnaryOp(Not, parsed_remainder), rest))
    }

    // Group
    [scanner.LParen, ..rest] -> {
      use #(body, rest) <- result.try(do_parse(rest))
      case rest {
        [scanner.RParen, ..rest] -> {
          case try_parse_binary_ops(rest) {
            Ok(#(op, rest)) -> Ok(#(op(Group(body)), rest))
            Error(_) -> Ok(#(Group(body), rest))
          }
        }
        _ -> Error(ParseError("missing closing parentheses"))
      }
    }

    [x, ..] -> Error(ParseError("Unexpected token: " <> string.inspect(x)))
  }
}

fn try_parse_binary_ops(
  tokens: List(scanner.Token),
) -> Result(#(fn(Expr) -> Expr, List(scanner.Token)), ParseError) {
  case tokens {
    [scanner.Plus, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, Add, rhs), rest))
    }
    [scanner.Minus, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, Subtract, rhs), rest))
    }
    [scanner.Star, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, Multiply, rhs), rest))
    }
    [scanner.Div, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, Divide, rhs), rest))
    }
    [scanner.StarStar, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, Power, rhs), rest))
    }
    [scanner.BangEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, NotEqualCheck, rhs), rest))
    }
    [scanner.EqualEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, EqualCheck, rhs), rest))
    }
    [scanner.LessEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, LessThanOrEqualCheck, rhs), rest))
    }
    [scanner.Less, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, LessThanCheck, rhs), rest))
    }
    [scanner.GreaterEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, GreaterThanOrEqualCheck, rhs), rest))
    }
    [scanner.Greater, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, GreaterThanCheck, rhs), rest))
    }
    [scanner.And, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, And, rhs), rest))
    }
    [scanner.Or, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      Ok(#(BinaryOp(_, Or, rhs), rest))
    }
    _ -> Error(ParseError("Not a binary operation"))
  }
}
