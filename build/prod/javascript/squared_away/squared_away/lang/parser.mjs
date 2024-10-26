import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList } from "../../gleam.mjs";
import * as $expr from "../../squared_away/lang/parser/expr.mjs";
import * as $parse_error from "../../squared_away/lang/parser/parse_error.mjs";
import * as $token from "../../squared_away/lang/scanner/token.mjs";

function try_parse_binary_ops(tokens) {
  if (tokens.atLeastLength(1) && tokens.head instanceof $token.Plus) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.Add(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Minus) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.Subtract(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Star) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.Multiply(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Div) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.Divide(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.StarStar) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.Power(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.BangEqual) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(
                _capture,
                new $expr.NotEqualCheck(),
                rhs,
              );
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.EqualEqual) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.EqualCheck(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.LessEqual) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(
                _capture,
                new $expr.LessThanOrEqualCheck(),
                rhs,
              );
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Less) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(
                _capture,
                new $expr.LessThanCheck(),
                rhs,
              );
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) &&
  tokens.head instanceof $token.GreaterEqual) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(
                _capture,
                new $expr.GreaterThanOrEqualCheck(),
                rhs,
              );
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Greater) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(
                _capture,
                new $expr.GreaterThanCheck(),
                rhs,
              );
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.And) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.And(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Or) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new $expr.BinaryOp(_capture, new $expr.Or(), rhs);
            },
            rest$1,
          ],
        );
      },
    );
  } else {
    return new Error(new $parse_error.ParseError("Not a binary operation"));
  }
}

function do_parse(tokens) {
  if (tokens.hasLength(0)) {
    return new Ok([new $expr.Empty(), toList([])]);
  } else if (tokens.atLeastLength(1) &&
  tokens.head instanceof $token.StringLiteral) {
    let str = tokens.head[0];
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new $expr.StringLiteral(str)), rest$1]);
    } else {
      return new Ok([new $expr.StringLiteral(str), rest]);
    }
  } else if (tokens.atLeastLength(1) &&
  tokens.head instanceof $token.IntegerLiteral) {
    let n = tokens.head.n;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new $expr.IntegerLiteral(n)), rest$1]);
    } else {
      return new Ok([new $expr.IntegerLiteral(n), rest]);
    }
  } else if (tokens.atLeastLength(1) &&
  tokens.head instanceof $token.FloatLiteral) {
    let f = tokens.head.f;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new $expr.FloatLiteral(f)), rest$1]);
    } else {
      return new Ok([new $expr.FloatLiteral(f), rest]);
    }
  } else if (tokens.atLeastLength(1) &&
  tokens.head instanceof $token.CellReference) {
    let key = tokens.head.key;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new $expr.CellReference(key)), rest$1]);
    } else {
      return new Ok([new $expr.CellReference(key), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.TrueToken) {
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new $expr.BooleanLiteral(true)), rest$1]);
    } else {
      return new Ok([new $expr.BooleanLiteral(true), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.FalseToken) {
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new $expr.BooleanLiteral(false)), rest$1]);
    } else {
      return new Ok([new $expr.BooleanLiteral(false), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Minus) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let parsed_remainder = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [new $expr.UnaryOp(new $expr.Negate(), parsed_remainder), rest$1],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.Bang) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let parsed_remainder = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [new $expr.UnaryOp(new $expr.Not(), parsed_remainder), rest$1],
        );
      },
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof $token.LParen) {
    let rest = tokens.tail;
    return $result.try$(
      do_parse(rest),
      (_use0) => {
        let body = _use0[0];
        let rest$1 = _use0[1];
        if (rest$1.atLeastLength(1) && rest$1.head instanceof $token.RParen) {
          let rest$2 = rest$1.tail;
          let $ = try_parse_binary_ops(rest$2);
          if ($.isOk()) {
            let op = $[0][0];
            let rest$3 = $[0][1];
            return new Ok([op(new $expr.Group(body)), rest$3]);
          } else {
            return new Ok([new $expr.Group(body), rest$2]);
          }
        } else {
          return new Error(
            new $parse_error.ParseError("missing closing parentheses"),
          );
        }
      },
    );
  } else {
    let x = tokens.head;
    return new Error(
      new $parse_error.ParseError("Unexpected token: " + $string.inspect(x)),
    );
  }
}

export function parse(tokens) {
  return $result.try$(
    do_parse(tokens),
    (_use0) => {
      let expr = _use0[0];
      let rest = _use0[1];
      if (rest.hasLength(0)) {
        return new Ok(expr);
      } else {
        return new Error(
          new $parse_error.ParseError(
            "After parsing there were leftover tokens",
          ),
        );
      }
    },
  );
}
