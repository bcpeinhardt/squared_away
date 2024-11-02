import gleam/bool
import gleam/io
import gleam/result
import gleam/string
import squared_away_lang/parser/expr
import squared_away_lang/parser/parse_error
import squared_away_lang/scanner/token

pub fn parse(
  tokens: List(token.Token),
) -> Result(expr.Expr, parse_error.ParseError) {
  use #(expr, rest) <- result.try(do_parse(tokens))
  case rest {
    [] -> Ok(expr)
    _ -> {
      io.debug(expr)
      io.debug(rest)
      Error(parse_error.ParseError(
        "After parsing there were leftover tokens " <> string.inspect(rest),
      ))
    }
  }
}

fn do_parse(
  tokens: List(token.Token),
) -> Result(#(expr.Expr, List(token.Token)), parse_error.ParseError) {
  case tokens {
    [] -> Ok(#(expr.Empty, []))
    [token.LabelDef(str), ..rest] -> Ok(#(expr.LabelDef(str), rest))
    [token.Label(row), token.Underscore, token.Label(col), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(expr.CrossLabel(row, col)), rest))
        Error(_) -> Ok(#(expr.CrossLabel(row, col), rest))
      }
    }
    [token.Label(str), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(expr.Label(str)), rest))
        Error(_) -> Ok(#(expr.Label(str), rest))
      }
    }
    [token.IntegerLiteral(n), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(expr.IntegerLiteral(n)), rest))
        Error(_) -> Ok(#(expr.IntegerLiteral(n), rest))
      }
    }
    [token.FloatLiteral(f), ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(expr.FloatLiteral(f)), rest))
        Error(_) -> Ok(#(expr.FloatLiteral(f), rest))
      }
    }
    [token.TrueToken, ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(expr.BooleanLiteral(True)), rest))
        Error(_) -> Ok(#(expr.BooleanLiteral(True), rest))
      }
    }
    [token.FalseToken, ..rest] -> {
      case try_parse_binary_ops(rest) {
        Ok(#(op, rest)) -> Ok(#(op(expr.BooleanLiteral(False)), rest))
        Error(_) -> Ok(#(expr.BooleanLiteral(False), rest))
      }
    }

    // Unary Ops
    [token.Minus, ..rest] -> {
      use #(parsed_remainder, rest) <- result.try(do_parse(rest))
      Ok(#(expr.UnaryOp(expr.Negate, parsed_remainder), rest))
    }
    [token.Bang, ..rest] -> {
      use #(parsed_remainder, rest) <- result.try(do_parse(rest))
      Ok(#(expr.UnaryOp(expr.Not, parsed_remainder), rest))
    }

    // Groupexpr.
    [token.LParen, ..rest] -> {
      use #(body, rest) <- result.try(do_parse(rest))
      case rest {
        [token.RParen, ..rest] -> {
          case try_parse_binary_ops(rest) {
            Ok(#(op, rest)) -> Ok(#(op(expr.Group(body)), rest))
            Error(_) -> Ok(#(expr.Group(body), rest))
          }
        }
        _ -> Error(parse_error.ParseError("missing closing parentheses"))
      }
    }

    [x, ..] ->
      Error(parse_error.ParseError("Unexpected token: " <> string.inspect(x)))
  }
}

fn try_parse_binary_ops(
  tokens: List(token.Token),
) -> Result(
  #(fn(expr.Expr) -> expr.Expr, List(token.Token)),
  parse_error.ParseError,
) {
  case tokens {
    [token.Plus, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.Add, rhs), rest))
    }
    [token.Minus, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.Subtract, rhs), rest))
    }
    [token.Star, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.Multiply, rhs), rest))
    }
    [token.Div, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.Divide, rhs), rest))
    }
    [token.StarStar, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.Power, rhs), rest))
    }
    [token.BangEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.NotEqualCheck, rhs), rest))
    }
    [token.EqualEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.EqualCheck, rhs), rest))
    }
    [token.LessEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.LessThanOrEqualCheck, rhs), rest))
    }
    [token.Less, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.LessThanCheck, rhs), rest))
    }
    [token.GreaterEqual, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.GreaterThanOrEqualCheck, rhs), rest))
    }
    [token.Greater, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.GreaterThanCheck, rhs), rest))
    }
    [token.And, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.And, rhs), rest))
    }
    [token.Or, ..rest] -> {
      use #(rhs, rest) <- result.try(do_parse(rest))
      use <- bool.guard(
        rhs == expr.Empty,
        Error(parse_error.ParseError(
          "No item on right hand side of binary operation.",
        )),
      )
      Ok(#(expr.BinaryOp(_, expr.Or, rhs), rest))
    }
    _ -> Error(parse_error.ParseError("Not a binary operation"))
  }
}
