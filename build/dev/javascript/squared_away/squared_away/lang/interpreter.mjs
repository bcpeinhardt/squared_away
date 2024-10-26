import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, makeError, divideFloat, divideInt } from "../../gleam.mjs";
import * as $error from "../../squared_away/lang/error.mjs";
import * as $value from "../../squared_away/lang/interpreter/value.mjs";
import * as $expr from "../../squared_away/lang/parser/expr.mjs";
import * as $typed_expr from "../../squared_away/lang/typechecker/typed_expr.mjs";

export function interpret(loop$env, loop$expr) {
  while (true) {
    let env = loop$env;
    let expr = loop$expr;
    if (expr instanceof $typed_expr.Empty) {
      return new Ok(new $value.Empty());
    } else if (expr instanceof $typed_expr.Group) {
      let expr$1 = expr.expr;
      loop$env = env;
      loop$expr = expr$1;
    } else if (expr instanceof $typed_expr.Label) {
      let txt = expr.txt;
      return new Ok(new $value.Text(txt));
    } else if (expr instanceof $typed_expr.BooleanLiteral) {
      let b = expr.b;
      return new Ok(new $value.Boolean(b));
    } else if (expr instanceof $typed_expr.IntegerLiteral) {
      let n = expr.n;
      return new Ok(new $value.Integer(n));
    } else if (expr instanceof $typed_expr.FloatLiteral) {
      let f = expr.f;
      return new Ok(new $value.FloatingPointNumber(f));
    } else if (expr instanceof $typed_expr.CellReference) {
      let cell_ref = expr.key;
      let $ = $dict.get(env, cell_ref);
      if (!$.isOk()) {
        throw makeError(
          "assignment_no_match",
          "squared_away/lang/interpreter",
          22,
          "interpret",
          "Assignment pattern did not match",
          { value: $ }
        )
      }
      let maybe_expr = $[0];
      if (!maybe_expr.isOk()) {
        let e = maybe_expr[0];
        return new Error(e);
      } else {
        let expr$1 = maybe_expr[0];
        loop$env = env;
        loop$expr = expr$1;
      }
    } else if (expr instanceof $typed_expr.UnaryOp) {
      let op = expr.op;
      let expr$1 = expr.expr;
      return $result.try$(
        interpret(env, expr$1),
        (value) => {
          if (op instanceof $expr.Negate && value instanceof $value.Integer) {
            let n = value.n;
            return new Ok(new $value.Integer(- n));
          } else if (op instanceof $expr.Negate &&
          value instanceof $value.FloatingPointNumber) {
            let f = value.f;
            return new Ok(new $value.FloatingPointNumber($float.negate(f)));
          } else if (op instanceof $expr.Not && value instanceof $value.Boolean) {
            let b = value.b;
            return new Ok(new $value.Boolean(!b));
          } else {
            throw makeError(
              "panic",
              "squared_away/lang/interpreter",
              36,
              "",
              "These should be the only options if the typechecker is working",
              {}
            )
          }
        },
      );
    } else {
      let lhs = expr.lhs;
      let op = expr.op;
      let rhs = expr.rhs;
      return $result.try$(
        interpret(env, lhs),
        (lhs) => {
          return $result.try$(
            interpret(env, rhs),
            (rhs) => {
              if (lhs instanceof $value.Integer &&
              op instanceof $expr.Add &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Integer(a + b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.Subtract &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Integer(a - b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.Multiply &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Integer(a * b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.Divide &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Integer(divideInt(a, b)));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.EqualCheck &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Boolean(a === b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.NotEqualCheck &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Boolean(a !== b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.GreaterThanCheck &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Boolean(a > b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.GreaterThanOrEqualCheck &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Boolean(a >= b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.LessThanCheck &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Boolean(a < b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.LessThanOrEqualCheck &&
              rhs instanceof $value.Integer) {
                let a = lhs.n;
                let b = rhs.n;
                return new Ok(new $value.Boolean(a <= b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.Add &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.FloatingPointNumber(a + b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.Subtract &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.FloatingPointNumber(a - b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.Multiply &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.FloatingPointNumber(a * b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.Divide &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.FloatingPointNumber(divideFloat(a, b)));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.EqualCheck &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.Boolean(a === b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.NotEqualCheck &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.Boolean(a !== b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.GreaterThanCheck &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.Boolean(a > b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.GreaterThanOrEqualCheck &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.Boolean(a >= b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.LessThanCheck &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.Boolean(a < b));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.LessThanOrEqualCheck &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                return new Ok(new $value.Boolean(a <= b));
              } else if (lhs instanceof $value.Integer &&
              op instanceof $expr.Power &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.n;
                let b = rhs.f;
                let $ = $int.power(a, b);
                if (!$.isOk()) {
                  throw makeError(
                    "assignment_no_match",
                    "squared_away/lang/interpreter",
                    104,
                    "",
                    "Assignment pattern did not match",
                    { value: $ }
                  )
                }
                let p = $[0];
                return new Ok(new $value.FloatingPointNumber(p));
              } else if (lhs instanceof $value.FloatingPointNumber &&
              op instanceof $expr.Power &&
              rhs instanceof $value.FloatingPointNumber) {
                let a = lhs.f;
                let b = rhs.f;
                let $ = $float.power(a, b);
                if (!$.isOk()) {
                  throw makeError(
                    "assignment_no_match",
                    "squared_away/lang/interpreter",
                    108,
                    "",
                    "Assignment pattern did not match",
                    { value: $ }
                  )
                }
                let p = $[0];
                return new Ok(new $value.FloatingPointNumber(p));
              } else if (lhs instanceof $value.Boolean &&
              op instanceof $expr.And &&
              rhs instanceof $value.Boolean) {
                let a = lhs.b;
                let b = rhs.b;
                return new Ok(new $value.Boolean(a && b));
              } else if (lhs instanceof $value.Boolean &&
              op instanceof $expr.Or &&
              rhs instanceof $value.Boolean) {
                let a = lhs.b;
                let b = rhs.b;
                return new Ok(new $value.Boolean(a || b));
              } else if (lhs instanceof $value.Boolean &&
              op instanceof $expr.EqualCheck &&
              rhs instanceof $value.Boolean) {
                let a = lhs.b;
                let b = rhs.b;
                return new Ok(new $value.Boolean(a === b));
              } else if (lhs instanceof $value.Boolean &&
              op instanceof $expr.NotEqualCheck &&
              rhs instanceof $value.Boolean) {
                let a = lhs.b;
                let b = rhs.b;
                return new Ok(new $value.Boolean(a !== b));
              } else {
                throw makeError(
                  "panic",
                  "squared_away/lang/interpreter",
                  122,
                  "",
                  "these should be the only options if the typechecker is working properly",
                  {}
                )
              }
            },
          );
        },
      );
    }
  }
}
