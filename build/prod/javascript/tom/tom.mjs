import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "./gleam.mjs";

export class Int extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Float extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Infinity extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Nan extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Bool extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class String extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Date extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Time extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class DateTime extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Array extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ArrayOfTables extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Table extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class InlineTable extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class DateTimeValue extends $CustomType {
  constructor(date, time, offset) {
    super();
    this.date = date;
    this.time = time;
    this.offset = offset;
  }
}

export class DateValue extends $CustomType {
  constructor(year, month, day) {
    super();
    this.year = year;
    this.month = month;
    this.day = day;
  }
}

export class TimeValue extends $CustomType {
  constructor(hour, minute, second, millisecond) {
    super();
    this.hour = hour;
    this.minute = minute;
    this.second = second;
    this.millisecond = millisecond;
  }
}

export class Local extends $CustomType {}

export class Offset extends $CustomType {
  constructor(direction, hours, minutes) {
    super();
    this.direction = direction;
    this.hours = hours;
    this.minutes = minutes;
  }
}

export class Positive extends $CustomType {}

export class Negative extends $CustomType {}

export class Unexpected extends $CustomType {
  constructor(got, expected) {
    super();
    this.got = got;
    this.expected = expected;
  }
}

export class KeyAlreadyInUse extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

export class NumberInt extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NumberFloat extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NumberInfinity extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NumberNan extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NotFound extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

export class WrongType extends $CustomType {
  constructor(key, expected, got) {
    super();
    this.key = key;
    this.expected = expected;
    this.got = got;
  }
}

function classify(toml) {
  if (toml instanceof Int) {
    return "Int";
  } else if (toml instanceof Float) {
    return "Float";
  } else if (toml instanceof Nan && toml[0] instanceof Positive) {
    return "NaN";
  } else if (toml instanceof Nan && toml[0] instanceof Negative) {
    return "Negative NaN";
  } else if (toml instanceof Infinity && toml[0] instanceof Positive) {
    return "Infinity";
  } else if (toml instanceof Infinity && toml[0] instanceof Negative) {
    return "Negative Infinity";
  } else if (toml instanceof Bool) {
    return "Bool";
  } else if (toml instanceof String) {
    return "String";
  } else if (toml instanceof Date) {
    return "Date";
  } else if (toml instanceof Time) {
    return "Time";
  } else if (toml instanceof DateTime) {
    return "DateTime";
  } else if (toml instanceof Array) {
    return "Array";
  } else if (toml instanceof ArrayOfTables) {
    return "Array";
  } else if (toml instanceof Table) {
    return "Table";
  } else {
    return "Table";
  }
}

function push_key(result, key) {
  if (result.isOk()) {
    let t = result[0];
    return new Ok(t);
  } else if (!result.isOk() && result[0] instanceof NotFound) {
    let path = result[0].key;
    return new Error(new NotFound(listPrepend(key, path)));
  } else {
    let path = result[0].key;
    let expected = result[0].expected;
    let got = result[0].got;
    return new Error(new WrongType(listPrepend(key, path), expected, got));
  }
}

export function get(toml, key) {
  if (key.hasLength(0)) {
    return new Error(new NotFound(toList([])));
  } else if (key.hasLength(1)) {
    let k = key.head;
    return $result.replace_error($dict.get(toml, k), new NotFound(toList([k])));
  } else {
    let k = key.head;
    let key$1 = key.tail;
    let $ = $dict.get(toml, k);
    if ($.isOk() && $[0] instanceof Table) {
      let t = $[0][0];
      return push_key(get(t, key$1), k);
    } else if ($.isOk() && $[0] instanceof InlineTable) {
      let t = $[0][0];
      return push_key(get(t, key$1), k);
    } else if ($.isOk()) {
      let other = $[0];
      return new Error(new WrongType(toList([k]), "Table", classify(other)));
    } else {
      return new Error(new NotFound(toList([k])));
    }
  }
}

export function get_int(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Int) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Int", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_float(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Float) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Float", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_bool(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Bool) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Bool", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_string(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof String) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "String", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_date(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Date) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Date", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_time(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Time) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Time", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_date_time(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof DateTime) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "DateTime", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_array(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Array) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk() && $[0] instanceof ArrayOfTables) {
    let i = $[0][0];
    return new Ok($list.map(i, (var0) => { return new Table(var0); }));
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Array", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_table(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Table) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk() && $[0] instanceof InlineTable) {
    let i = $[0][0];
    return new Ok(i);
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Table", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

export function get_number(toml, key) {
  let $ = get(toml, key);
  if ($.isOk() && $[0] instanceof Int) {
    let x = $[0][0];
    return new Ok(new NumberInt(x));
  } else if ($.isOk() && $[0] instanceof Float) {
    let x = $[0][0];
    return new Ok(new NumberFloat(x));
  } else if ($.isOk() && $[0] instanceof Nan) {
    let x = $[0][0];
    return new Ok(new NumberNan(x));
  } else if ($.isOk() && $[0] instanceof Infinity) {
    let x = $[0][0];
    return new Ok(new NumberInfinity(x));
  } else if ($.isOk()) {
    let other = $[0];
    return new Error(new WrongType(key, "Number", classify(other)));
  } else {
    let e = $[0];
    return new Error(e);
  }
}

function merge(table, key, old, new$) {
  if (old instanceof ArrayOfTables && new$ instanceof ArrayOfTables) {
    let tables = old[0];
    let new$1 = new$[0];
    return new Ok(
      $dict.insert(table, key, new ArrayOfTables($list.append(new$1, tables))),
    );
  } else {
    return new Error(toList([key]));
  }
}

function insert_loop(table, key, value) {
  if (key.hasLength(0)) {
    throw makeError("panic", "tom", 515, "insert_loop", "unreachable", {})
  } else if (key.hasLength(1)) {
    let k = key.head;
    let $ = $dict.get(table, k);
    if (!$.isOk() && !$[0]) {
      return new Ok($dict.insert(table, k, value));
    } else {
      let old = $[0];
      return merge(table, k, old, value);
    }
  } else {
    let k = key.head;
    let key$1 = key.tail;
    let $ = $dict.get(table, k);
    if (!$.isOk() && !$[0]) {
      let $1 = insert_loop($dict.new$(), key$1, value);
      if ($1.isOk()) {
        let inner = $1[0];
        return new Ok($dict.insert(table, k, new Table(inner)));
      } else {
        let path = $1[0];
        return new Error(listPrepend(k, path));
      }
    } else if ($.isOk() &&
    $[0] instanceof ArrayOfTables &&
    $[0][0].atLeastLength(1)) {
      let inner = $[0][0].head;
      let rest = $[0][0].tail;
      let $1 = insert_loop(inner, key$1, value);
      if ($1.isOk()) {
        let inner$1 = $1[0];
        return new Ok(
          $dict.insert(table, k, new ArrayOfTables(listPrepend(inner$1, rest))),
        );
      } else {
        let path = $1[0];
        return new Error(listPrepend(k, path));
      }
    } else if ($.isOk() && $[0] instanceof Table) {
      let inner = $[0][0];
      let $1 = insert_loop(inner, key$1, value);
      if ($1.isOk()) {
        let inner$1 = $1[0];
        return new Ok($dict.insert(table, k, new Table(inner$1)));
      } else {
        let path = $1[0];
        return new Error(listPrepend(k, path));
      }
    } else {
      return new Error(toList([k]));
    }
  }
}

function insert(table, key, value) {
  let $ = insert_loop(table, key, value);
  if ($.isOk()) {
    let table$1 = $[0];
    return new Ok(table$1);
  } else {
    let path = $[0];
    return new Error(new KeyAlreadyInUse(path));
  }
}

function expect_end_of_line(input, next) {
  if (input.atLeastLength(1) && input.head === "\n") {
    let input$1 = input.tail;
    return next(input$1);
  } else if (input.atLeastLength(1) && input.head === "\r\n") {
    let input$1 = input.tail;
    return next(input$1);
  } else if (input.atLeastLength(1)) {
    let g = input.head;
    return new Error(new Unexpected(g, "\n"));
  } else {
    return new Error(new Unexpected("EOF", "\n"));
  }
}

function parse_key_quoted(loop$input, loop$close, loop$name) {
  while (true) {
    let input = loop$input;
    let close = loop$close;
    let name = loop$name;
    if (input.atLeastLength(1) && (input.head === close)) {
      let g = input.head;
      let input$1 = input.tail;
      return new Ok([name, input$1]);
    } else if (input.atLeastLength(1)) {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$close = close;
      loop$name = name + g;
    } else {
      return new Error(new Unexpected("EOF", close));
    }
  }
}

function parse_key_bare(loop$input, loop$name) {
  while (true) {
    let input = loop$input;
    let name = loop$name;
    if (input.atLeastLength(1) && input.head === " " && (name !== "")) {
      let input$1 = input.tail;
      return new Ok([name, input$1]);
    } else if (input.atLeastLength(1) && input.head === "=" && (name !== "")) {
      return new Ok([name, input]);
    } else if (input.atLeastLength(1) && input.head === "." && (name !== "")) {
      return new Ok([name, input]);
    } else if (input.atLeastLength(1) && input.head === "]" && (name !== "")) {
      return new Ok([name, input]);
    } else if (input.atLeastLength(1) && input.head === "," && (name !== "")) {
      return new Error(new Unexpected(",", "="));
    } else if (input.atLeastLength(1) && input.head === "\n" && (name !== "")) {
      return new Error(new Unexpected("\n", "="));
    } else if (input.atLeastLength(1) && input.head === "\r\n" && (name !== "")) {
      return new Error(new Unexpected("\r\n", "="));
    } else if (input.atLeastLength(1) && input.head === "\n") {
      return new Error(new Unexpected("\n", "key"));
    } else if (input.atLeastLength(1) && input.head === "\r\n") {
      return new Error(new Unexpected("\r\n", "key"));
    } else if (input.atLeastLength(1) && input.head === "]") {
      return new Error(new Unexpected("]", "key"));
    } else if (input.atLeastLength(1) && input.head === ",") {
      return new Error(new Unexpected(",", "key"));
    } else if (input.atLeastLength(1)) {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$name = name + g;
    } else {
      return new Error(new Unexpected("EOF", "key"));
    }
  }
}

function skip_line_whitespace(input) {
  return $list.drop_while(input, (g) => { return (g === " ") || (g === "\t"); });
}

function parse_key_segment(input) {
  let input$1 = skip_line_whitespace(input);
  if (input$1.atLeastLength(1) && input$1.head === "=") {
    return new Error(new Unexpected("=", "Key"));
  } else if (input$1.atLeastLength(1) && input$1.head === "\n") {
    return new Error(new Unexpected("\n", "Key"));
  } else if (input$1.atLeastLength(1) && input$1.head === "\r\n") {
    return new Error(new Unexpected("\r\n", "Key"));
  } else if (input$1.atLeastLength(1) && input$1.head === "[") {
    return new Error(new Unexpected("[", "Key"));
  } else if (input$1.atLeastLength(1) && input$1.head === "\"") {
    let input$2 = input$1.tail;
    return parse_key_quoted(input$2, "\"", "");
  } else if (input$1.atLeastLength(1) && input$1.head === "'") {
    let input$2 = input$1.tail;
    return parse_key_quoted(input$2, "'", "");
  } else {
    return parse_key_bare(input$1, "");
  }
}

function skip_whitespace(loop$input) {
  while (true) {
    let input = loop$input;
    if (input.atLeastLength(1) && input.head === " ") {
      let input$1 = input.tail;
      loop$input = input$1;
    } else if (input.atLeastLength(1) && input.head === "\t") {
      let input$1 = input.tail;
      loop$input = input$1;
    } else if (input.atLeastLength(1) && input.head === "\n") {
      let input$1 = input.tail;
      loop$input = input$1;
    } else if (input.atLeastLength(1) && input.head === "\r\n") {
      let input$1 = input.tail;
      loop$input = input$1;
    } else {
      let input$1 = input;
      return input$1;
    }
  }
}

function drop_comments(loop$input, loop$acc, loop$in_string) {
  while (true) {
    let input = loop$input;
    let acc = loop$acc;
    let in_string = loop$in_string;
    if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\"" &&
    (in_string)) {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$acc = listPrepend("\"", listPrepend("\\", acc));
      loop$in_string = in_string;
    } else if (input.atLeastLength(1) && input.head === "\"") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$acc = listPrepend("\"", acc);
      loop$in_string = !in_string;
    } else if (input.atLeastLength(1) && input.head === "#" && (in_string)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$acc = listPrepend("#", acc);
      loop$in_string = in_string;
    } else if (input.atLeastLength(1) && input.head === "#" && (!in_string)) {
      let input$1 = input.tail;
      let _pipe = input$1;
      let _pipe$1 = $list.drop_while(_pipe, (g) => { return g !== "\n"; });
      loop$input = _pipe$1;
      loop$acc = acc;
      loop$in_string = in_string;
    } else if (input.atLeastLength(1)) {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$acc = listPrepend(g, acc);
      loop$in_string = in_string;
    } else {
      return $list.reverse(acc);
    }
  }
}

function do$(result, next) {
  if (result.isOk()) {
    let a = result[0][0];
    let input = result[0][1];
    return next(a, input);
  } else {
    let e = result[0];
    return new Error(e);
  }
}

function parse_key(input, segments) {
  return do$(
    parse_key_segment(input),
    (segment, input) => {
      let segments$1 = listPrepend(segment, segments);
      let input$1 = skip_line_whitespace(input);
      if (input$1.atLeastLength(1) && input$1.head === ".") {
        let input$2 = input$1.tail;
        return parse_key(input$2, segments$1);
      } else {
        return new Ok([$list.reverse(segments$1), input$1]);
      }
    },
  );
}

function expect(input, expected, next) {
  if (input.atLeastLength(1) && (input.head === expected)) {
    let g = input.head;
    let input$1 = input.tail;
    return next(input$1);
  } else if (input.atLeastLength(1)) {
    let g = input.head;
    return new Error(new Unexpected(g, expected));
  } else {
    return new Error(new Unexpected("EOF", expected));
  }
}

function parse_table_header(input) {
  let input$1 = skip_line_whitespace(input);
  return do$(
    parse_key(input$1, toList([])),
    (key, input) => {
      return expect(
        input,
        "]",
        (input) => {
          let input$1 = skip_line_whitespace(input);
          return expect_end_of_line(
            input$1,
            (input) => { return new Ok([key, input]); },
          );
        },
      );
    },
  );
}

function parse_hex(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input.atLeastLength(1) && input.head === "_") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "0") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 0;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "1") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 1;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "2") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 2;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "3") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 3;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "4") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 4;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "5") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 5;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "6") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 6;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "7") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 7;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "8") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 8;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "9") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 9;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "a") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 10;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "b") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 11;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "c") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 12;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "d") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 13;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "e") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 14;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "f") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 15;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "A") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 10;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "B") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 11;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "C") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 12;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "D") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 13;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "E") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 14;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "F") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 16 + 15;
      loop$sign = sign;
    } else {
      let input$1 = input;
      let number$1 = (() => {
        if (sign instanceof Positive) {
          return number;
        } else {
          return - number;
        }
      })();
      return new Ok([new Int(number$1), input$1]);
    }
  }
}

function parse_octal(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input.atLeastLength(1) && input.head === "_") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "0") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 0;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "1") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 1;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "2") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 2;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "3") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 3;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "4") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 4;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "5") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 5;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "6") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 6;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "7") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 8 + 7;
      loop$sign = sign;
    } else {
      let input$1 = input;
      let number$1 = (() => {
        if (sign instanceof Positive) {
          return number;
        } else {
          return - number;
        }
      })();
      return new Ok([new Int(number$1), input$1]);
    }
  }
}

function parse_binary(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input.atLeastLength(1) && input.head === "_") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "0") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 2 + 0;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "1") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 2 + 1;
      loop$sign = sign;
    } else {
      let input$1 = input;
      let number$1 = (() => {
        if (sign instanceof Positive) {
          return number;
        } else {
          return - number;
        }
      })();
      return new Ok([new Int(number$1), input$1]);
    }
  }
}

function parse_exponent(loop$input, loop$n, loop$n_sign, loop$ex, loop$ex_sign) {
  while (true) {
    let input = loop$input;
    let n = loop$n;
    let n_sign = loop$n_sign;
    let ex = loop$ex;
    let ex_sign = loop$ex_sign;
    if (input.atLeastLength(1) && input.head === "_") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "0") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "1") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 1;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "2") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 2;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "3") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 3;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "4") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 4;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "5") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 5;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "6") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 6;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "7") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 7;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "8") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 8;
      loop$ex_sign = ex_sign;
    } else if (input.atLeastLength(1) && input.head === "9") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$n = n;
      loop$n_sign = n_sign;
      loop$ex = ex * 10 + 9;
      loop$ex_sign = ex_sign;
    } else {
      let input$1 = input;
      let number = (() => {
        if (n_sign instanceof Positive) {
          return n;
        } else {
          return n * -1.0;
        }
      })();
      let exponent = $int.to_float(
        (() => {
          if (ex_sign instanceof Positive) {
            return ex;
          } else {
            return - ex;
          }
        })(),
      );
      let multiplier = (() => {
        let $ = $float.power(10.0, exponent);
        if ($.isOk()) {
          let multiplier = $[0];
          return multiplier;
        } else {
          return 1.0;
        }
      })();
      return new Ok([new Float(number * multiplier), input$1]);
    }
  }
}

function parse_float(loop$input, loop$number, loop$sign, loop$unit) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    let unit = loop$unit;
    if (input.atLeastLength(1) && input.head === "_") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number;
      loop$sign = sign;
      loop$unit = unit;
    } else if (input.atLeastLength(1) && input.head === "0") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number;
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "1") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (1.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "2") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (2.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "3") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (3.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "4") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (4.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "5") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (5.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "6") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (6.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "7") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (7.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "8") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (8.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(1) && input.head === "9") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number + (9.0 * unit);
      loop$sign = sign;
      loop$unit = unit * 0.1;
    } else if (input.atLeastLength(2) &&
    input.head === "e" &&
    input.tail.head === "+") {
      let input$1 = input.tail.tail;
      return parse_exponent(input$1, number, sign, 0, new Positive());
    } else if (input.atLeastLength(2) &&
    input.head === "e" &&
    input.tail.head === "-") {
      let input$1 = input.tail.tail;
      return parse_exponent(input$1, number, sign, 0, new Negative());
    } else if (input.atLeastLength(1) && input.head === "e") {
      let input$1 = input.tail;
      return parse_exponent(input$1, number, sign, 0, new Positive());
    } else if (input.atLeastLength(2) &&
    input.head === "E" &&
    input.tail.head === "+") {
      let input$1 = input.tail.tail;
      return parse_exponent(input$1, number, sign, 0, new Positive());
    } else if (input.atLeastLength(2) &&
    input.head === "E" &&
    input.tail.head === "-") {
      let input$1 = input.tail.tail;
      return parse_exponent(input$1, number, sign, 0, new Negative());
    } else if (input.atLeastLength(1) && input.head === "E") {
      let input$1 = input.tail;
      return parse_exponent(input$1, number, sign, 0, new Positive());
    } else {
      let input$1 = input;
      let number$1 = (() => {
        if (sign instanceof Positive) {
          return number;
        } else {
          return number * -1.0;
        }
      })();
      return new Ok([new Float(number$1), input$1]);
    }
  }
}

function parse_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input.atLeastLength(1) && input.head === "\"") {
      let input$1 = input.tail;
      return new Ok([new String(string), input$1]);
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "t") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\t";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "e") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\u{001b}";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "b") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\u{0008}";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "n") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\n";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "r") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\r";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "f") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\f";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\"") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\"";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\\") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\\";
    } else if (input.hasLength(0)) {
      return new Error(new Unexpected("EOF", "\""));
    } else if (input.atLeastLength(1) && input.head === "\n") {
      return new Error(new Unexpected("\n", "\""));
    } else if (input.atLeastLength(1) && input.head === "\r\n") {
      return new Error(new Unexpected("\r\n", "\""));
    } else {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string + g;
    }
  }
}

function parse_multi_line_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input.atLeastLength(3) &&
    input.head === "\"" &&
    input.tail.head === "\"" &&
    input.tail.tail.head === "\"") {
      let input$1 = input.tail.tail.tail;
      return new Ok([new String(string), input$1]);
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\n") {
      let input$1 = input.tail.tail;
      loop$input = skip_whitespace(input$1);
      loop$string = string;
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\r\n") {
      let input$1 = input.tail.tail;
      loop$input = skip_whitespace(input$1);
      loop$string = string;
    } else if (input.atLeastLength(1) &&
    input.head === "\r\n" &&
    (string === "")) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string;
    } else if (input.atLeastLength(1) && input.head === "\n" && (string === "")) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string;
    } else if (input.atLeastLength(1) &&
    input.head === "\r\n" &&
    (string === "")) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string;
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "t") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\t";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "n") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\n";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "r") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\r";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\"") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\"";
    } else if (input.atLeastLength(2) &&
    input.head === "\\" &&
    input.tail.head === "\\") {
      let input$1 = input.tail.tail;
      loop$input = input$1;
      loop$string = string + "\\";
    } else if (input.hasLength(0)) {
      return new Error(new Unexpected("EOF", "\""));
    } else {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string + g;
    }
  }
}

function parse_multi_line_literal_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input.hasLength(0)) {
      return new Error(new Unexpected("EOF", "\""));
    } else if (input.atLeastLength(4) &&
    input.head === "'" &&
    input.tail.head === "'" &&
    input.tail.tail.head === "'" &&
    input.tail.tail.tail.head === "'") {
      return new Error(new Unexpected("''''", "'''"));
    } else if (input.atLeastLength(3) &&
    input.head === "'" &&
    input.tail.head === "'" &&
    input.tail.tail.head === "'") {
      let input$1 = input.tail.tail.tail;
      return new Ok([new String(string), input$1]);
    } else if (input.atLeastLength(1) && input.head === "\n" && (string === "")) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string;
    } else if (input.atLeastLength(1) &&
    input.head === "\r\n" &&
    (string === "")) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string;
    } else {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string + g;
    }
  }
}

function parse_literal_string(loop$input, loop$string) {
  while (true) {
    let input = loop$input;
    let string = loop$string;
    if (input.hasLength(0)) {
      return new Error(new Unexpected("EOF", "\""));
    } else if (input.atLeastLength(1) && input.head === "\n") {
      return new Error(new Unexpected("\n", "'"));
    } else if (input.atLeastLength(1) && input.head === "\r\n") {
      return new Error(new Unexpected("\r\n", "'"));
    } else if (input.atLeastLength(1) && input.head === "'") {
      let input$1 = input.tail;
      return new Ok([new String(string), input$1]);
    } else {
      let g = input.head;
      let input$1 = input.tail;
      loop$input = input$1;
      loop$string = string + g;
    }
  }
}

function parse_time_ms(loop$input, loop$seconds, loop$ms) {
  while (true) {
    let input = loop$input;
    let seconds = loop$seconds;
    let ms = loop$ms;
    if (input.atLeastLength(1) && input.head === "0" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 0;
    } else if (input.atLeastLength(1) && input.head === "1" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 1;
    } else if (input.atLeastLength(1) && input.head === "2" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 2;
    } else if (input.atLeastLength(1) && input.head === "3" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 3;
    } else if (input.atLeastLength(1) && input.head === "4" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 4;
    } else if (input.atLeastLength(1) && input.head === "5" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 5;
    } else if (input.atLeastLength(1) && input.head === "6" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 6;
    } else if (input.atLeastLength(1) && input.head === "7" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 7;
    } else if (input.atLeastLength(1) && input.head === "8" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 8;
    } else if (input.atLeastLength(1) && input.head === "9" && (ms < 100_000)) {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$seconds = seconds;
      loop$ms = ms * 10 + 9;
    } else {
      return new Ok([[seconds, ms], input]);
    }
  }
}

function parse_number_under_60(input, expected) {
  if (input.atLeastLength(2) && input.head === "0" && input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return new Ok([0, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return new Ok([1, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return new Ok([2, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return new Ok([3, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return new Ok([4, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return new Ok([5, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return new Ok([6, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return new Ok([7, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return new Ok([8, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return new Ok([9, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return new Ok([10, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return new Ok([11, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return new Ok([12, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return new Ok([13, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return new Ok([14, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return new Ok([15, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return new Ok([16, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return new Ok([17, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return new Ok([18, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return new Ok([19, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return new Ok([20, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return new Ok([21, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return new Ok([22, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return new Ok([23, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return new Ok([24, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return new Ok([25, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return new Ok([26, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return new Ok([27, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return new Ok([28, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return new Ok([29, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return new Ok([30, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return new Ok([31, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return new Ok([32, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return new Ok([33, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return new Ok([34, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return new Ok([35, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return new Ok([36, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return new Ok([37, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return new Ok([38, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return new Ok([39, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return new Ok([40, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return new Ok([41, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return new Ok([42, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return new Ok([43, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return new Ok([44, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return new Ok([45, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return new Ok([46, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return new Ok([47, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return new Ok([48, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "4" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return new Ok([49, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return new Ok([50, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return new Ok([51, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return new Ok([52, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return new Ok([53, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return new Ok([54, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return new Ok([55, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return new Ok([56, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return new Ok([57, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return new Ok([58, input$1]);
  } else if (input.atLeastLength(2) &&
  input.head === "5" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return new Ok([59, input$1]);
  } else if (input.atLeastLength(1)) {
    let g = input.head;
    return new Error(new Unexpected(g, expected));
  } else {
    return new Error(new Unexpected("EOF", expected));
  }
}

function parse_hour_minute(input) {
  return do$(
    (() => {
      if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "0" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([0, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "1" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([1, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "2" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([2, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "3" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([3, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "4" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([4, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "5" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([5, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "6" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([6, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "7" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([7, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "8" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([8, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "0" &&
      input.tail.head === "9" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([9, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "0" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([10, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "1" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([11, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "2" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([12, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "3" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([13, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "4" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([14, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "5" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([15, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "6" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([16, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "7" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([17, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "8" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([18, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "1" &&
      input.tail.head === "9" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([19, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "2" &&
      input.tail.head === "0" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([20, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "2" &&
      input.tail.head === "1" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([21, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "2" &&
      input.tail.head === "2" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([22, input$1]);
      } else if (input.atLeastLength(3) &&
      input.head === "2" &&
      input.tail.head === "3" &&
      input.tail.tail.head === ":") {
        let input$1 = input.tail.tail.tail;
        return new Ok([23, input$1]);
      } else if (input.atLeastLength(1)) {
        let g = input.head;
        return new Error(new Unexpected(g, "time"));
      } else {
        return new Error(new Unexpected("EOF", "time"));
      }
    })(),
    (hours, input) => {
      return do$(
        parse_number_under_60(input, "minutes"),
        (minutes, input) => { return new Ok([[hours, minutes], input]); },
      );
    },
  );
}

function parse_time_s_ms(input) {
  if (input.atLeastLength(1) && input.head === ":") {
    let input$1 = input.tail;
    return do$(
      parse_number_under_60(input$1, "seconds"),
      (seconds, input) => {
        if (input.atLeastLength(1) && input.head === ".") {
          let input$1 = input.tail;
          return parse_time_ms(input$1, seconds, 0);
        } else {
          return new Ok([[seconds, 0], input]);
        }
      },
    );
  } else {
    return new Ok([[0, 0], input]);
  }
}

function parse_time_minute(input, hours) {
  return do$(
    parse_number_under_60(input, "minutes"),
    (minutes, input) => {
      return do$(
        parse_time_s_ms(input),
        (_use0, input) => {
          let seconds = _use0[0];
          let ms = _use0[1];
          let time = new TimeValue(hours, minutes, seconds, ms);
          return new Ok([new Time(time), input]);
        },
      );
    },
  );
}

function parse_time_value(input) {
  return do$(
    parse_hour_minute(input),
    (_use0, input) => {
      let hours = _use0[0];
      let minutes = _use0[1];
      return do$(
        parse_time_s_ms(input),
        (_use0, input) => {
          let seconds = _use0[0];
          let ms = _use0[1];
          let time = new TimeValue(hours, minutes, seconds, ms);
          return new Ok([time, input]);
        },
      );
    },
  );
}

function parse_offset_hours(input, sign) {
  return do$(
    parse_hour_minute(input),
    (_use0, input) => {
      let hours = _use0[0];
      let minutes = _use0[1];
      return new Ok([new Offset(sign, hours, minutes), input]);
    },
  );
}

function parse_offset(input) {
  if (input.atLeastLength(1) && input.head === "Z") {
    let input$1 = input.tail;
    return new Ok([new Offset(new Positive(), 0, 0), input$1]);
  } else if (input.atLeastLength(1) && input.head === "+") {
    let input$1 = input.tail;
    return parse_offset_hours(input$1, new Positive());
  } else if (input.atLeastLength(1) && input.head === "-") {
    let input$1 = input.tail;
    return parse_offset_hours(input$1, new Negative());
  } else {
    return new Ok([new Local(), input]);
  }
}

function parse_date_end(input, year, month, day) {
  let date = new DateValue(year, month, day);
  if (input.atLeastLength(1) && input.head === " ") {
    let input$1 = input.tail;
    return do$(
      parse_time_value(input$1),
      (time, input) => {
        return do$(
          parse_offset(input),
          (offset, input) => {
            return new Ok(
              [new DateTime(new DateTimeValue(date, time, offset)), input],
            );
          },
        );
      },
    );
  } else if (input.atLeastLength(1) && input.head === "T") {
    let input$1 = input.tail;
    return do$(
      parse_time_value(input$1),
      (time, input) => {
        return do$(
          parse_offset(input),
          (offset, input) => {
            return new Ok(
              [new DateTime(new DateTimeValue(date, time, offset)), input],
            );
          },
        );
      },
    );
  } else {
    return new Ok([new Date(date), input]);
  }
}

function parse_date_day(input, year, month) {
  if (input.atLeastLength(2) && input.head === "0" && input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 1);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 2);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 3);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 4);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 5);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 6);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 7);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 8);
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 9);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 10);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 11);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 12);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 13);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 14);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 15);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 16);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 17);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 18);
  } else if (input.atLeastLength(2) &&
  input.head === "1" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 19);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 20);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 21);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "2") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 22);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "3") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 23);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "4") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 24);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "5") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 25);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "6") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 26);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "7") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 27);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "8") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 28);
  } else if (input.atLeastLength(2) &&
  input.head === "2" &&
  input.tail.head === "9") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 29);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "0") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 30);
  } else if (input.atLeastLength(2) &&
  input.head === "3" &&
  input.tail.head === "1") {
    let input$1 = input.tail.tail;
    return parse_date_end(input$1, year, month, 31);
  } else if (input.atLeastLength(1)) {
    let g = input.head;
    return new Error(new Unexpected(g, "date day"));
  } else {
    return new Error(new Unexpected("EOF", "date day"));
  }
}

function parse_date(input, year) {
  if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "1" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 1);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "2" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 2);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "3" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 3);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "4" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 4);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "5" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 5);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "6" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 6);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "7" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 7);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "8" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 8);
  } else if (input.atLeastLength(3) &&
  input.head === "0" &&
  input.tail.head === "9" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 9);
  } else if (input.atLeastLength(3) &&
  input.head === "1" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 10);
  } else if (input.atLeastLength(3) &&
  input.head === "1" &&
  input.tail.head === "1" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 11);
  } else if (input.atLeastLength(3) &&
  input.head === "1" &&
  input.tail.head === "2" &&
  input.tail.tail.head === "-") {
    let input$1 = input.tail.tail.tail;
    return parse_date_day(input$1, year, 12);
  } else if (input.atLeastLength(1)) {
    let g = input.head;
    return new Error(new Unexpected(g, "date month"));
  } else {
    return new Error(new Unexpected("EOF", "date month"));
  }
}

function parse_number(loop$input, loop$number, loop$sign) {
  while (true) {
    let input = loop$input;
    let number = loop$number;
    let sign = loop$sign;
    if (input.atLeastLength(1) && input.head === "_") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "0") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 0;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "1") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 1;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "2") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 2;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "3") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 3;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "4") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 4;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "5") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 5;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "6") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 6;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "7") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 7;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "8") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 8;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "9") {
      let input$1 = input.tail;
      loop$input = input$1;
      loop$number = number * 10 + 9;
      loop$sign = sign;
    } else if (input.atLeastLength(1) && input.head === "-") {
      let input$1 = input.tail;
      return parse_date(input$1, number);
    } else if (input.atLeastLength(1) && input.head === ":" && (number < 24)) {
      let input$1 = input.tail;
      return parse_time_minute(input$1, number);
    } else if (input.atLeastLength(1) && input.head === ".") {
      let input$1 = input.tail;
      return parse_float(input$1, $int.to_float(number), sign, 0.1);
    } else if (input.atLeastLength(2) &&
    input.head === "e" &&
    input.tail.head === "+") {
      let input$1 = input.tail.tail;
      return parse_exponent(
        input$1,
        $int.to_float(number),
        sign,
        0,
        new Positive(),
      );
    } else if (input.atLeastLength(2) &&
    input.head === "e" &&
    input.tail.head === "-") {
      let input$1 = input.tail.tail;
      return parse_exponent(
        input$1,
        $int.to_float(number),
        sign,
        0,
        new Negative(),
      );
    } else if (input.atLeastLength(1) && input.head === "e") {
      let input$1 = input.tail;
      return parse_exponent(
        input$1,
        $int.to_float(number),
        sign,
        0,
        new Positive(),
      );
    } else if (input.atLeastLength(2) &&
    input.head === "E" &&
    input.tail.head === "+") {
      let input$1 = input.tail.tail;
      return parse_exponent(
        input$1,
        $int.to_float(number),
        sign,
        0,
        new Positive(),
      );
    } else if (input.atLeastLength(2) &&
    input.head === "E" &&
    input.tail.head === "-") {
      let input$1 = input.tail.tail;
      return parse_exponent(
        input$1,
        $int.to_float(number),
        sign,
        0,
        new Negative(),
      );
    } else if (input.atLeastLength(1) && input.head === "E") {
      let input$1 = input.tail;
      return parse_exponent(
        input$1,
        $int.to_float(number),
        sign,
        0,
        new Positive(),
      );
    } else {
      let input$1 = input;
      let number$1 = (() => {
        if (sign instanceof Positive) {
          return number;
        } else {
          return - number;
        }
      })();
      return new Ok([new Int(number$1), input$1]);
    }
  }
}

function reverse_arrays_of_tables(toml) {
  if (toml instanceof ArrayOfTables) {
    let tables = toml[0];
    return new ArrayOfTables(reverse_arrays_of_tables_array(tables, toList([])));
  } else if (toml instanceof Table) {
    let table = toml[0];
    return new Table(reverse_arrays_of_tables_table(table));
  } else {
    return toml;
  }
}

function reverse_arrays_of_tables_table(table) {
  return $dict.map_values(
    table,
    (_, v) => { return reverse_arrays_of_tables(v); },
  );
}

function reverse_arrays_of_tables_array(loop$array, loop$acc) {
  while (true) {
    let array = loop$array;
    let acc = loop$acc;
    if (array.hasLength(0)) {
      return acc;
    } else {
      let first = array.head;
      let rest = array.tail;
      let first$1 = reverse_arrays_of_tables_table(first);
      loop$array = rest;
      loop$acc = listPrepend(first$1, acc);
    }
  }
}

function parse_inline_table_property(input, properties) {
  let input$1 = skip_whitespace(input);
  return do$(
    parse_key(input$1, toList([])),
    (key, input) => {
      let input$1 = skip_line_whitespace(input);
      return expect(
        input$1,
        "=",
        (input) => {
          let input$1 = skip_line_whitespace(input);
          return do$(
            parse_value(input$1),
            (value, input) => {
              let $ = insert(properties, key, value);
              if ($.isOk()) {
                let properties$1 = $[0];
                return new Ok([properties$1, input]);
              } else {
                let e = $[0];
                return new Error(e);
              }
            },
          );
        },
      );
    },
  );
}

function parse_value(input) {
  if (input.atLeastLength(4) &&
  input.head === "t" &&
  input.tail.head === "r" &&
  input.tail.tail.head === "u" &&
  input.tail.tail.tail.head === "e") {
    let input$1 = input.tail.tail.tail.tail;
    return new Ok([new Bool(true), input$1]);
  } else if (input.atLeastLength(5) &&
  input.head === "f" &&
  input.tail.head === "a" &&
  input.tail.tail.head === "l" &&
  input.tail.tail.tail.head === "s" &&
  input.tail.tail.tail.tail.head === "e") {
    let input$1 = input.tail.tail.tail.tail.tail;
    return new Ok([new Bool(false), input$1]);
  } else if (input.atLeastLength(3) &&
  input.head === "n" &&
  input.tail.head === "a" &&
  input.tail.tail.head === "n") {
    let input$1 = input.tail.tail.tail;
    return new Ok([new Nan(new Positive()), input$1]);
  } else if (input.atLeastLength(4) &&
  input.head === "+" &&
  input.tail.head === "n" &&
  input.tail.tail.head === "a" &&
  input.tail.tail.tail.head === "n") {
    let input$1 = input.tail.tail.tail.tail;
    return new Ok([new Nan(new Positive()), input$1]);
  } else if (input.atLeastLength(4) &&
  input.head === "-" &&
  input.tail.head === "n" &&
  input.tail.tail.head === "a" &&
  input.tail.tail.tail.head === "n") {
    let input$1 = input.tail.tail.tail.tail;
    return new Ok([new Nan(new Negative()), input$1]);
  } else if (input.atLeastLength(3) &&
  input.head === "i" &&
  input.tail.head === "n" &&
  input.tail.tail.head === "f") {
    let input$1 = input.tail.tail.tail;
    return new Ok([new Infinity(new Positive()), input$1]);
  } else if (input.atLeastLength(4) &&
  input.head === "+" &&
  input.tail.head === "i" &&
  input.tail.tail.head === "n" &&
  input.tail.tail.tail.head === "f") {
    let input$1 = input.tail.tail.tail.tail;
    return new Ok([new Infinity(new Positive()), input$1]);
  } else if (input.atLeastLength(4) &&
  input.head === "-" &&
  input.tail.head === "i" &&
  input.tail.tail.head === "n" &&
  input.tail.tail.tail.head === "f") {
    let input$1 = input.tail.tail.tail.tail;
    return new Ok([new Infinity(new Negative()), input$1]);
  } else if (input.atLeastLength(1) && input.head === "[") {
    let input$1 = input.tail;
    return parse_array(input$1, toList([]));
  } else if (input.atLeastLength(1) && input.head === "{") {
    let input$1 = input.tail;
    return parse_inline_table(input$1, $dict.new$());
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "x") {
    let input$1 = input.tail.tail;
    return parse_hex(input$1, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "+" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "x") {
    let input$1 = input.tail.tail.tail;
    return parse_hex(input$1, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "-" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "x") {
    let input$1 = input.tail.tail.tail;
    return parse_hex(input$1, 0, new Negative());
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "o") {
    let input$1 = input.tail.tail;
    return parse_octal(input$1, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "+" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "o") {
    let input$1 = input.tail.tail.tail;
    return parse_octal(input$1, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "-" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "o") {
    let input$1 = input.tail.tail.tail;
    return parse_octal(input$1, 0, new Negative());
  } else if (input.atLeastLength(2) &&
  input.head === "0" &&
  input.tail.head === "b") {
    let input$1 = input.tail.tail;
    return parse_binary(input$1, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "+" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "b") {
    let input$1 = input.tail.tail.tail;
    return parse_binary(input$1, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "-" &&
  input.tail.head === "0" &&
  input.tail.tail.head === "b") {
    let input$1 = input.tail.tail.tail;
    return parse_binary(input$1, 0, new Negative());
  } else if (input.atLeastLength(1) && input.head === "+") {
    let input$1 = input.tail;
    return parse_number(input$1, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "-") {
    let input$1 = input.tail;
    return parse_number(input$1, 0, new Negative());
  } else if (input.atLeastLength(1) && input.head === "0") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "1") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "2") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "3") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "4") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "5") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "6") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "7") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "8") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(1) && input.head === "9") {
    return parse_number(input, 0, new Positive());
  } else if (input.atLeastLength(3) &&
  input.head === "\"" &&
  input.tail.head === "\"" &&
  input.tail.tail.head === "\"") {
    let input$1 = input.tail.tail.tail;
    return parse_multi_line_string(input$1, "");
  } else if (input.atLeastLength(1) && input.head === "\"") {
    let input$1 = input.tail;
    return parse_string(input$1, "");
  } else if (input.atLeastLength(3) &&
  input.head === "'" &&
  input.tail.head === "'" &&
  input.tail.tail.head === "'") {
    let input$1 = input.tail.tail.tail;
    return parse_multi_line_literal_string(input$1, "");
  } else if (input.atLeastLength(1) && input.head === "'") {
    let input$1 = input.tail;
    return parse_literal_string(input$1, "");
  } else if (input.atLeastLength(1)) {
    let g = input.head;
    return new Error(new Unexpected(g, "value"));
  } else {
    return new Error(new Unexpected("EOF", "value"));
  }
}

function parse_inline_table(loop$input, loop$properties) {
  while (true) {
    let input = loop$input;
    let properties = loop$properties;
    let input$1 = skip_whitespace(input);
    if (input$1.atLeastLength(1) && input$1.head === "}") {
      let input$2 = input$1.tail;
      return new Ok([new InlineTable(properties), input$2]);
    } else {
      let $ = parse_inline_table_property(input$1, properties);
      if ($.isOk()) {
        let properties$1 = $[0][0];
        let input$2 = $[0][1];
        let input$3 = skip_whitespace(input$2);
        if (input$3.atLeastLength(1) && input$3.head === "}") {
          let input$4 = input$3.tail;
          return new Ok([new InlineTable(properties$1), input$4]);
        } else if (input$3.atLeastLength(1) && input$3.head === ",") {
          let input$4 = input$3.tail;
          let input$5 = skip_whitespace(input$4);
          loop$input = input$5;
          loop$properties = properties$1;
        } else if (input$3.atLeastLength(1)) {
          let g = input$3.head;
          return new Error(new Unexpected(g, "}"));
        } else {
          return new Error(new Unexpected("EOF", "}"));
        }
      } else {
        let e = $[0];
        return new Error(e);
      }
    }
  }
}

function parse_key_value(input, toml) {
  return do$(
    parse_key(input, toList([])),
    (key, input) => {
      let input$1 = skip_line_whitespace(input);
      return expect(
        input$1,
        "=",
        (input) => {
          let input$1 = skip_line_whitespace(input);
          return do$(
            parse_value(input$1),
            (value, input) => {
              let $ = insert(toml, key, value);
              if ($.isOk()) {
                let toml$1 = $[0];
                return new Ok([toml$1, input]);
              } else {
                let e = $[0];
                return new Error(e);
              }
            },
          );
        },
      );
    },
  );
}

function parse_table(loop$input, loop$toml) {
  while (true) {
    let input = loop$input;
    let toml = loop$toml;
    let input$1 = skip_whitespace(input);
    if (input$1.atLeastLength(1) && input$1.head === "[") {
      return new Ok([toml, input$1]);
    } else if (input$1.hasLength(0)) {
      return new Ok([toml, input$1]);
    } else {
      let $ = parse_key_value(input$1, toml);
      if ($.isOk()) {
        let toml$1 = $[0][0];
        let input$2 = $[0][1];
        let $1 = skip_line_whitespace(input$2);
        if ($1.hasLength(0)) {
          return new Ok([toml$1, toList([])]);
        } else if ($1.atLeastLength(1) && $1.head === "\n") {
          let in$ = $1.tail;
          loop$input = in$;
          loop$toml = toml$1;
        } else if ($1.atLeastLength(1) && $1.head === "\r\n") {
          let in$ = $1.tail;
          loop$input = in$;
          loop$toml = toml$1;
        } else {
          let g = $1.head;
          return new Error(new Unexpected(g, "\n"));
        }
      } else {
        let e = $;
        return e;
      }
    }
  }
}

function parse_array_of_tables(input) {
  let input$1 = skip_line_whitespace(input);
  return do$(
    parse_key(input$1, toList([])),
    (key, input) => {
      return expect(
        input,
        "]",
        (input) => {
          return expect(
            input,
            "]",
            (input) => {
              return do$(
                parse_table(input, $dict.new$()),
                (table, input) => { return new Ok([[key, table], input]); },
              );
            },
          );
        },
      );
    },
  );
}

function parse_table_and_header(input) {
  return do$(
    parse_table_header(input),
    (key, input) => {
      return do$(
        parse_table(input, $dict.new$()),
        (table, input) => { return new Ok([[key, table], input]); },
      );
    },
  );
}

function parse_tables(loop$input, loop$toml) {
  while (true) {
    let input = loop$input;
    let toml = loop$toml;
    if (input.atLeastLength(2) && input.head === "[" && input.tail.head === "[") {
      let input$1 = input.tail.tail;
      let $ = parse_array_of_tables(input$1);
      if (!$.isOk()) {
        let e = $[0];
        return new Error(e);
      } else {
        let key = $[0][0][0];
        let table = $[0][0][1];
        let input$2 = $[0][1];
        let $1 = insert(toml, key, new ArrayOfTables(toList([table])));
        if ($1.isOk()) {
          let toml$1 = $1[0];
          loop$input = input$2;
          loop$toml = toml$1;
        } else {
          let e = $1[0];
          return new Error(e);
        }
      }
    } else if (input.atLeastLength(1) && input.head === "[") {
      let input$1 = input.tail;
      let $ = parse_table_and_header(input$1);
      if (!$.isOk()) {
        let e = $[0];
        return new Error(e);
      } else {
        let key = $[0][0][0];
        let table = $[0][0][1];
        let input$2 = $[0][1];
        let $1 = insert(toml, key, new Table(table));
        if ($1.isOk()) {
          let toml$1 = $1[0];
          loop$input = input$2;
          loop$toml = toml$1;
        } else {
          let e = $1[0];
          return new Error(e);
        }
      }
    } else if (input.atLeastLength(1)) {
      let g = input.head;
      return new Error(new Unexpected(g, "["));
    } else {
      return new Ok(toml);
    }
  }
}

export function parse(input) {
  let input$1 = $string.to_graphemes(input);
  let input$2 = drop_comments(input$1, toList([]), false);
  let input$3 = skip_whitespace(input$2);
  return do$(
    parse_table(input$3, $dict.new$()),
    (toml, input) => {
      let $ = parse_tables(input, toml);
      if ($.isOk()) {
        let toml$1 = $[0];
        return new Ok(reverse_arrays_of_tables_table(toml$1));
      } else {
        let e = $[0];
        return new Error(e);
      }
    },
  );
}

function parse_array(input, elements) {
  let input$1 = skip_whitespace(input);
  if (input$1.atLeastLength(1) && input$1.head === "]") {
    let input$2 = input$1.tail;
    return new Ok([new Array($list.reverse(elements)), input$2]);
  } else {
    return do$(
      parse_value(input$1),
      (element, input) => {
        let elements$1 = listPrepend(element, elements);
        let input$1 = skip_whitespace(input);
        if (input$1.atLeastLength(1) && input$1.head === "]") {
          let input$2 = input$1.tail;
          return new Ok([new Array($list.reverse(elements$1)), input$2]);
        } else if (input$1.atLeastLength(1) && input$1.head === ",") {
          let input$2 = input$1.tail;
          let input$3 = skip_whitespace(input$2);
          return parse_array(input$3, elements$1);
        } else if (input$1.atLeastLength(1)) {
          let g = input$1.head;
          return new Error(new Unexpected(g, "]"));
        } else {
          return new Error(new Unexpected("EOF", "]"));
        }
      },
    );
  }
}
