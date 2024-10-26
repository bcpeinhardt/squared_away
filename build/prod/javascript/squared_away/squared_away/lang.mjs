import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Error } from "../gleam.mjs";
import * as $error from "../squared_away/lang/error.mjs";
import * as $interpreter from "../squared_away/lang/interpreter.mjs";
import * as $value from "../squared_away/lang/interpreter/value.mjs";
import * as $parser from "../squared_away/lang/parser.mjs";
import * as $expr from "../squared_away/lang/parser/expr.mjs";
import * as $scanner from "../squared_away/lang/scanner.mjs";
import * as $token from "../squared_away/lang/scanner/token.mjs";
import * as $typechecker from "../squared_away/lang/typechecker.mjs";
import * as $typed_expr from "../squared_away/lang/typechecker/typed_expr.mjs";

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
