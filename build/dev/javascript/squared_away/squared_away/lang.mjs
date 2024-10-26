import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, makeError } from "../gleam.mjs";
import * as $error from "../squared_away/lang/error.mjs";
import * as $interpreter from "../squared_away/lang/interpreter.mjs";
import * as $value from "../squared_away/lang/interpreter/value.mjs";
import * as $parser from "../squared_away/lang/parser.mjs";
import * as $expr from "../squared_away/lang/parser/expr.mjs";
import * as $scanner from "../squared_away/lang/scanner.mjs";
import * as $token from "../squared_away/lang/scanner/token.mjs";
import * as $typechecker from "../squared_away/lang/typechecker.mjs";
import * as $typ from "../squared_away/lang/typechecker/typ.mjs";
import * as $typed_expr from "../squared_away/lang/typechecker/typed_expr.mjs";
import * as $util from "../squared_away/util.mjs";

export function interpret_grid(input) {
  return $dict.fold(
    input,
    $dict.new$(),
    (acc, key, typed_expr) => {
      if (!typed_expr.isOk()) {
        let e = typed_expr[0];
        return $dict.insert(acc, key, new Error(e));
      } else {
        let typed_expr$1 = typed_expr[0];
        let maybe_value = $interpreter.interpret(input, typed_expr$1);
        return $dict.insert(acc, key, maybe_value);
      }
    },
  );
}

export function typecheck_grid(input) {
  return $dict.fold(
    input,
    $dict.new$(),
    (acc, key, expr) => {
      if (!expr.isOk()) {
        let e = expr[0];
        return $dict.insert(acc, key, new Error(e));
      } else if (expr.isOk() && expr[0] instanceof $expr.Label) {
        let txt = expr[0].txt;
        let $ = $util.cell_to_the_right(key);
        if ($ instanceof None) {
          return $dict.insert(
            acc,
            key,
            new Ok(new $typed_expr.Label(new $typ.TNil(), txt)),
          );
        } else {
          let new_key = $[0];
          let $1 = $dict.get(input, new_key);
          if (!$1.isOk()) {
            throw makeError(
              "assignment_no_match",
              "squared_away/lang",
              41,
              "",
              "Assignment pattern did not match",
              { value: $1 }
            )
          }
          let val = $1[0];
          if (!val.isOk()) {
            let e = val[0];
            return $dict.insert(acc, key, new Error(e));
          } else {
            let val$1 = val[0];
            let $2 = $typechecker.typecheck(input, val$1);
            if (!$2.isOk()) {
              let e = $2[0];
              return $dict.insert(acc, key, new Error(e));
            } else {
              let typed_val = $2[0];
              return $dict.insert(
                acc,
                key,
                new Ok(new $typed_expr.Label(typed_val.type_, txt)),
              );
            }
          }
        }
      } else {
        let expr$1 = expr[0];
        let maybe_typed_expr = $typechecker.typecheck(input, expr$1);
        return $dict.insert(acc, key, maybe_typed_expr);
      }
    },
  );
}

export function parse_grid(input) {
  return $dict.fold(
    input,
    $dict.new$(),
    (acc, key, toks) => {
      if (!toks.isOk()) {
        let e = toks[0];
        return $dict.insert(acc, key, new Error(e));
      } else {
        let toks$1 = toks[0];
        let expr = $parser.parse(toks$1);
        return $dict.insert(
          acc,
          key,
          (() => {
            let _pipe = expr;
            return $result.map_error(
              _pipe,
              (var0) => { return new $error.ParseError(var0); },
            );
          })(),
        );
      }
    },
  );
}

export function scan_grid(input) {
  return $dict.fold(
    input,
    $dict.new$(),
    (acc, key, src) => {
      let maybe_scanned = (() => {
        let _pipe = $scanner.scan(src);
        return $result.map_error(
          _pipe,
          (var0) => { return new $error.ScanError(var0); },
        );
      })();
      return $dict.insert(acc, key, maybe_scanned);
    },
  );
}
