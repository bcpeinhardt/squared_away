import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, makeError, isEqual } from "../../gleam.mjs";
import * as $error from "../../squared_away/lang/error.mjs";
import * as $expr from "../../squared_away/lang/parser/expr.mjs";
import * as $typ from "../../squared_away/lang/typechecker/typ.mjs";
import * as $type_error from "../../squared_away/lang/typechecker/type_error.mjs";
import * as $typed_expr from "../../squared_away/lang/typechecker/typed_expr.mjs";

export function typecheck(env, expr) {
  if (expr instanceof $expr.Empty) {
    return new Ok(new $typed_expr.Empty(new $typ.TNil()));
  } else if (expr instanceof $expr.StringLiteral) {
    let txt = expr.txt;
    return new Ok(new $typed_expr.StringLiteral(new $typ.TString(), txt));
  } else if (expr instanceof $expr.BooleanLiteral) {
    let b = expr.val;
    return new Ok(new $typed_expr.BooleanLiteral(new $typ.TBool(), b));
  } else if (expr instanceof $expr.FloatLiteral) {
    let f = expr.f;
    return new Ok(new $typed_expr.FloatLiteral(new $typ.TFloat(), f));
  } else if (expr instanceof $expr.IntegerLiteral) {
    let n = expr.n;
    return new Ok(new $typed_expr.IntegerLiteral(new $typ.TInt(), n));
  } else if (expr instanceof $expr.Group) {
    let inner = expr.inner;
    return $result.try$(
      typecheck(env, inner),
      (expr) => { return new Ok(new $typed_expr.Group(expr.type_, expr)); },
    );
  } else if (expr instanceof $expr.CellReference) {
    let key = expr.key;
    let $ = $dict.get(env, key);
    if (!$.isOk()) {
      throw makeError(
        "assignment_no_match",
        "squared_away/lang/typechecker",
        27,
        "typecheck",
        "Assignment pattern did not match",
        { value: $ }
      )
    }
    let ref_expr = $[0];
    if (ref_expr.isOk()) {
      let expr$1 = ref_expr[0];
      return $result.try$(
        typecheck(env, expr$1),
        (expr) => {
          return new Ok(new $typed_expr.CellReference(expr.type_, key));
        },
      );
    } else {
      let e = ref_expr[0];
      return new Error(e);
    }
  } else if (expr instanceof $expr.UnaryOp) {
    let op = expr.op;
    let expr$1 = expr.expr;
    return $result.try$(
      typecheck(env, expr$1),
      (expr) => {
        let $ = expr.type_;
        if (op instanceof $expr.Negate && $ instanceof $typ.TInt) {
          return new Ok(new $typed_expr.UnaryOp(expr.type_, op, expr));
        } else if (op instanceof $expr.Negate && $ instanceof $typ.TFloat) {
          return new Ok(new $typed_expr.UnaryOp(expr.type_, op, expr));
        } else if (op instanceof $expr.Not && $ instanceof $typ.TBool) {
          return new Ok(new $typed_expr.UnaryOp(expr.type_, op, expr));
        } else {
          return new Error(
            new $error.TypeError(
              new $type_error.TypeError(
                "Unexpected type and operator combination",
              ),
            ),
          );
        }
      },
    );
  } else {
    let lhs = expr.lhs;
    let op = expr.op;
    let rhs = expr.rhs;
    return $result.try$(
      typecheck(env, lhs),
      (lhs) => {
        return $result.try$(
          typecheck(env, rhs),
          (rhs) => {
            let $ = lhs.type_;
            let $1 = rhs.type_;
            if ($ instanceof $typ.TFloat &&
            op instanceof $expr.Add &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TFloat(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.Add &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TInt(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.Subtract &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TFloat(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.Subtract &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TInt(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.Multiply &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TFloat(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.Multiply &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TInt(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.Divide &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TFloat(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.Divide &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TInt(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.Power &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TFloat(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.Power &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TFloat(), lhs, op, rhs),
              );
            } else if (op instanceof $expr.EqualCheck && (isEqual($, $1))) {
              let t1 = $;
              let t2 = $1;
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if (op instanceof $expr.NotEqualCheck && (isEqual($, $1))) {
              let t1 = $;
              let t2 = $1;
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.LessThanCheck &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.LessThanOrEqualCheck &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.GreaterThanOrEqualCheck &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TFloat &&
            op instanceof $expr.GreaterThanCheck &&
            $1 instanceof $typ.TFloat) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.LessThanCheck &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.LessThanOrEqualCheck &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.GreaterThanOrEqualCheck &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TInt &&
            op instanceof $expr.GreaterThanCheck &&
            $1 instanceof $typ.TInt) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TString &&
            op instanceof $expr.LessThanCheck &&
            $1 instanceof $typ.TString) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TString &&
            op instanceof $expr.LessThanOrEqualCheck &&
            $1 instanceof $typ.TString) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TString &&
            op instanceof $expr.GreaterThanOrEqualCheck &&
            $1 instanceof $typ.TString) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TString &&
            op instanceof $expr.GreaterThanCheck &&
            $1 instanceof $typ.TString) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TBool &&
            op instanceof $expr.And &&
            $1 instanceof $typ.TBool) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TBool &&
            op instanceof $expr.Or &&
            $1 instanceof $typ.TBool) {
              return new Ok(
                new $typed_expr.BinaryOp(new $typ.TBool(), lhs, op, rhs),
              );
            } else if ($ instanceof $typ.TBool && op instanceof $expr.And) {
              let t = $1;
              if (t instanceof $typ.TNil) {
                return new Error(
                  new $error.TypeError(
                    new $type_error.TypeError(
                      "Tried to do a boolean and operation \"&&\" but the right hand side of the operation has type \"Empty\". Could you be referencing an empty cell?",
                    ),
                  ),
                );
              } else {
                return new Error(
                  new $error.TypeError(
                    new $type_error.TypeError(
                      "Tried to do a boolean and operation (b1 && b2) but the right hand side has type " + $string.inspect(
                        t,
                      ),
                    ),
                  ),
                );
              }
            } else {
              return new Error(
                new $error.TypeError(
                  new $type_error.TypeError(
                    "Unexpected arguments to binary operation: " + $string.inspect(
                      op,
                    ),
                  ),
                ),
              );
            }
          },
        );
      },
    );
  }
}
