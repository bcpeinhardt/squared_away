import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, prepend as listPrepend, makeError } from "../../gleam.mjs";
import * as $scan_error from "../../squared_away/lang/scanner/scan_error.mjs";
import * as $token from "../../squared_away/lang/scanner/token.mjs";

function parse_integer(loop$src, loop$acc) {
  while (true) {
    let src = loop$src;
    let acc = loop$acc;
    if (src.startsWith("1")) {
      let rest = src.slice(1);
      let x = "1";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("2")) {
      let rest = src.slice(1);
      let x = "2";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("3")) {
      let rest = src.slice(1);
      let x = "3";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("4")) {
      let rest = src.slice(1);
      let x = "4";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("5")) {
      let rest = src.slice(1);
      let x = "5";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("6")) {
      let rest = src.slice(1);
      let x = "6";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("7")) {
      let rest = src.slice(1);
      let x = "7";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("8")) {
      let rest = src.slice(1);
      let x = "8";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("9")) {
      let rest = src.slice(1);
      let x = "9";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("0")) {
      let rest = src.slice(1);
      let x = "0";
      loop$src = rest;
      loop$acc = acc + x;
    } else {
      let _pipe = $int.parse(acc);
      return $result.map(_pipe, (n) => { return [n, src]; });
    }
  }
}

function parse_cell_ref(loop$src, loop$acc) {
  while (true) {
    let src = loop$src;
    let acc = loop$acc;
    if (src.startsWith("A")) {
      let rest = src.slice(1);
      let l = "A";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("B")) {
      let rest = src.slice(1);
      let l = "B";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("C")) {
      let rest = src.slice(1);
      let l = "C";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("D")) {
      let rest = src.slice(1);
      let l = "D";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("E")) {
      let rest = src.slice(1);
      let l = "E";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("F")) {
      let rest = src.slice(1);
      let l = "F";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("G")) {
      let rest = src.slice(1);
      let l = "G";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("H")) {
      let rest = src.slice(1);
      let l = "H";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("I")) {
      let rest = src.slice(1);
      let l = "I";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("J")) {
      let rest = src.slice(1);
      let l = "J";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("K")) {
      let rest = src.slice(1);
      let l = "K";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("L")) {
      let rest = src.slice(1);
      let l = "L";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("M")) {
      let rest = src.slice(1);
      let l = "M";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("N")) {
      let rest = src.slice(1);
      let l = "N";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("O")) {
      let rest = src.slice(1);
      let l = "O";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("P")) {
      let rest = src.slice(1);
      let l = "P";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("Q")) {
      let rest = src.slice(1);
      let l = "Q";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("R")) {
      let rest = src.slice(1);
      let l = "R";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("S")) {
      let rest = src.slice(1);
      let l = "S";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("T")) {
      let rest = src.slice(1);
      let l = "T";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("U")) {
      let rest = src.slice(1);
      let l = "U";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("V")) {
      let rest = src.slice(1);
      let l = "V";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("W")) {
      let rest = src.slice(1);
      let l = "W";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("X")) {
      let rest = src.slice(1);
      let l = "X";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("Y")) {
      let rest = src.slice(1);
      let l = "Y";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("Z")) {
      let rest = src.slice(1);
      let l = "Z";
      loop$src = rest;
      loop$acc = acc + l;
    } else {
      if (acc === "") {
        return new Error(undefined);
      } else {
        return $result.try$(
          parse_integer(src, ""),
          (_use0) => {
            let n = _use0[0];
            let rest = _use0[1];
            return new Ok([acc + $int.to_string(n), rest]);
          },
        );
      }
    }
  }
}

function do_scan(loop$src, loop$acc) {
  while (true) {
    let src = loop$src;
    let acc = loop$acc;
    if (src === "") {
      return new Ok(
        (() => {
          let _pipe = acc;
          return $list.reverse(_pipe);
        })(),
      );
    } else if (src.startsWith("TRUE")) {
      let rest = src.slice(4);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.TrueToken(), acc);
    } else if (src.startsWith("FALSE")) {
      let rest = src.slice(5);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.FalseToken(), acc);
    } else if (src.startsWith("&&")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.And(), acc);
    } else if (src.startsWith("||")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Or(), acc);
    } else if (src.startsWith("**")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.StarStar(), acc);
    } else if (src.startsWith("==")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.EqualEqual(), acc);
    } else if (src.startsWith("!=")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.BangEqual(), acc);
    } else if (src.startsWith("<=")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.LessEqual(), acc);
    } else if (src.startsWith(">=")) {
      let rest = src.slice(2);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.GreaterEqual(), acc);
    } else if (src.startsWith("+")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Plus(), acc);
    } else if (src.startsWith("-")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Minus(), acc);
    } else if (src.startsWith("*")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Star(), acc);
    } else if (src.startsWith("/")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Div(), acc);
    } else if (src.startsWith("=")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Equal(), acc);
    } else if (src.startsWith("!")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Bang(), acc);
    } else if (src.startsWith("<")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Less(), acc);
    } else if (src.startsWith(">")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.Greater(), acc);
    } else if (src.startsWith("(")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.LParen(), acc);
    } else if (src.startsWith(")")) {
      let rest = src.slice(1);
      loop$src = $string.trim_left(rest);
      loop$acc = listPrepend(new $token.RParen(), acc);
    } else {
      let $ = parse_integer(src, "");
      if ($.isOk()) {
        let n = $[0][0];
        let rest = $[0][1];
        if (rest.startsWith(".")) {
          let rest$1 = rest.slice(1);
          return $result.try$(
            (() => {
              let _pipe = parse_integer(rest$1, "");
              return $result.replace_error(_pipe, new $scan_error.ScanError());
            })(),
            (_use0) => {
              let m = _use0[0];
              let rest$2 = _use0[1];
              let $1 = $float.parse(
                ($int.to_string(n) + ".") + $int.to_string(m),
              );
              if (!$1.isOk()) {
                throw makeError(
                  "assignment_no_match",
                  "squared_away/lang/scanner",
                  54,
                  "",
                  "Assignment pattern did not match",
                  { value: $1 }
                )
              }
              let f = $1[0];
              return do_scan(
                $string.trim_left(rest$2),
                listPrepend(new $token.FloatLiteral(f), acc),
              );
            },
          );
        } else {
          loop$src = $string.trim_left(rest);
          loop$acc = listPrepend(new $token.IntegerLiteral(n), acc);
        }
      } else {
        return $result.try$(
          (() => {
            let _pipe = parse_cell_ref(src, "");
            return $result.replace_error(_pipe, new $scan_error.ScanError());
          })(),
          (_use0) => {
            let cell_ref = _use0[0];
            let rest = _use0[1];
            return do_scan(
              $string.trim_left(rest),
              listPrepend(new $token.CellReference(cell_ref), acc),
            );
          },
        );
      }
    }
  }
}

export function scan(src) {
  let $ = $string.trim(src);
  if ($ === "") {
    return new Ok(toList([]));
  } else if ($.startsWith("=")) {
    let rest = $.slice(1);
    return do_scan(
      (() => {
        let _pipe = rest;
        return $string.trim_left(_pipe);
      })(),
      toList([]),
    );
  } else {
    return new Ok(toList([new $token.StringLiteral(src)]));
  }
}
