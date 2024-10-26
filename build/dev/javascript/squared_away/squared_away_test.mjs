import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $gleeunit from "../gleeunit/gleeunit.mjs";
import * as $should from "../gleeunit/gleeunit/should.mjs";
import { toList, makeError } from "./gleam.mjs";
import * as $interpreter from "./squared_away/lang/interpreter.mjs";
import * as $value from "./squared_away/lang/interpreter/value.mjs";
import * as $parser from "./squared_away/lang/parser.mjs";
import * as $expr from "./squared_away/lang/parser/expr.mjs";
import * as $scanner from "./squared_away/lang/scanner.mjs";
import * as $token from "./squared_away/lang/scanner/token.mjs";
import * as $typechecker from "./squared_away/lang/typechecker.mjs";

export function main() {
  return $gleeunit.main();
}

export function scanner_test() {
  let test_cases = toList([
    [
      "=-+*/",
      toList([
        new $token.Minus(),
        new $token.Plus(),
        new $token.Star(),
        new $token.Div(),
      ]),
    ],
    [
      "=   - >= + * / ** = ! && || != ( <= ) == <> TRUE FALSE",
      toList([
        new $token.Minus(),
        new $token.GreaterEqual(),
        new $token.Plus(),
        new $token.Star(),
        new $token.Div(),
        new $token.StarStar(),
        new $token.Equal(),
        new $token.Bang(),
        new $token.And(),
        new $token.Or(),
        new $token.BangEqual(),
        new $token.LParen(),
        new $token.LessEqual(),
        new $token.RParen(),
        new $token.EqualEqual(),
        new $token.Less(),
        new $token.Greater(),
        new $token.TrueToken(),
        new $token.FalseToken(),
      ]),
    ],
    [
      "=   - AS45 + 786",
      toList([
        new $token.Minus(),
        new $token.CellReference("AS45"),
        new $token.Plus(),
        new $token.IntegerLiteral(786),
      ]),
    ],
    ["+-*/=", toList([new $token.Label("+-*/=")])],
  ]);
  return $list.each(
    test_cases,
    (tc) => {
      let _pipe = tc[0];
      let _pipe$1 = $scanner.scan(_pipe);
      let _pipe$2 = $should.be_ok(_pipe$1);
      return $should.equal(_pipe$2, tc[1]);
    },
  );
}

export function parser_test() {
  let test_cases = toList([
    [
      toList([
        new $token.IntegerLiteral(7),
        new $token.Plus(),
        new $token.IntegerLiteral(8),
        new $token.Minus(),
        new $token.IntegerLiteral(9),
      ]),
      new $expr.BinaryOp(
        new $expr.IntegerLiteral(7),
        new $expr.Add(),
        new $expr.BinaryOp(
          new $expr.IntegerLiteral(8),
          new $expr.Subtract(),
          new $expr.IntegerLiteral(9),
        ),
      ),
    ],
    [
      toList([
        new $token.IntegerLiteral(1),
        new $token.Plus(),
        new $token.IntegerLiteral(2),
        new $token.Plus(),
        new $token.IntegerLiteral(3),
        new $token.Plus(),
        new $token.IntegerLiteral(4),
        new $token.Plus(),
        new $token.IntegerLiteral(5),
        new $token.Plus(),
        new $token.IntegerLiteral(6),
      ]),
      new $expr.BinaryOp(
        new $expr.IntegerLiteral(1),
        new $expr.Add(),
        new $expr.BinaryOp(
          new $expr.IntegerLiteral(2),
          new $expr.Add(),
          new $expr.BinaryOp(
            new $expr.IntegerLiteral(3),
            new $expr.Add(),
            new $expr.BinaryOp(
              new $expr.IntegerLiteral(4),
              new $expr.Add(),
              new $expr.BinaryOp(
                new $expr.IntegerLiteral(5),
                new $expr.Add(),
                new $expr.IntegerLiteral(6),
              ),
            ),
          ),
        ),
      ),
    ],
  ]);
  return $list.each(
    test_cases,
    (tc) => {
      let _pipe = tc[0];
      let _pipe$1 = $parser.parse(_pipe);
      let _pipe$2 = $should.be_ok(_pipe$1);
      return $should.equal(_pipe$2, tc[1]);
    },
  );
}

export function integration_lang_test() {
  let test_cases = toList([
    ["=27+4-10", new $value.Integer(21)],
    ["=TRUE && FALSE", new $value.Boolean(false)],
    ["=2+(5*8)", new $value.Integer(42)],
  ]);
  return $list.each(
    test_cases,
    (tc) => {
      let $ = $scanner.scan(tc[0]);
      if (!$.isOk()) {
        throw makeError(
          "assignment_no_match",
          "squared_away_test",
          124,
          "",
          "Assignment pattern did not match",
          { value: $ }
        )
      }
      let tokens = $[0];
      let $1 = $parser.parse(tokens);
      if (!$1.isOk()) {
        throw makeError(
          "assignment_no_match",
          "squared_away_test",
          125,
          "",
          "Assignment pattern did not match",
          { value: $1 }
        )
      }
      let expr = $1[0];
      let $2 = $typechecker.typecheck($dict.new$(), expr);
      if (!$2.isOk()) {
        throw makeError(
          "assignment_no_match",
          "squared_away_test",
          126,
          "",
          "Assignment pattern did not match",
          { value: $2 }
        )
      }
      let typed_expr = $2[0];
      let _pipe = $interpreter.interpret($dict.new$(), typed_expr);
      let _pipe$1 = $should.be_ok(_pipe);
      return $should.equal(_pipe$1, tc[1]);
    },
  );
}
