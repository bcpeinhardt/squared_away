import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string_builder from "../../gleam_stdlib/gleam/string_builder.mjs";
import { Error, CustomType as $CustomType } from "../gleam.mjs";
import {
  decode as decode_string,
  json_to_string as do_to_string,
  json_to_string as do_to_string_builder,
  identity as do_string,
  identity as do_bool,
  identity as do_int,
  identity as do_float,
  do_null,
  object as do_object,
  array as do_preprocessed_array,
} from "../gleam_json_ffi.mjs";

export class UnexpectedEndOfInput extends $CustomType {}

export class UnexpectedByte extends $CustomType {
  constructor(byte, position) {
    super();
    this.byte = byte;
    this.position = position;
  }
}

export class UnexpectedSequence extends $CustomType {
  constructor(byte, position) {
    super();
    this.byte = byte;
    this.position = position;
  }
}

export class UnexpectedFormat extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function do_decode(json, decoder) {
  return $result.then$(
    decode_string(json),
    (dynamic_value) => {
      let _pipe = decoder(dynamic_value);
      return $result.map_error(
        _pipe,
        (var0) => { return new UnexpectedFormat(var0); },
      );
    },
  );
}

export function decode(json, decoder) {
  return do_decode(json, decoder);
}

function decode_to_dynamic(json) {
  let $ = $bit_array.to_string(json);
  if ($.isOk()) {
    let string$1 = $[0];
    return decode_string(string$1);
  } else {
    return new Error(new UnexpectedByte("", 0));
  }
}

export function decode_bits(json, decoder) {
  return $result.then$(
    decode_to_dynamic(json),
    (dynamic_value) => {
      let _pipe = decoder(dynamic_value);
      return $result.map_error(
        _pipe,
        (var0) => { return new UnexpectedFormat(var0); },
      );
    },
  );
}

export function to_string(json) {
  return do_to_string(json);
}

export function to_string_builder(json) {
  return do_to_string_builder(json);
}

export function string(input) {
  return do_string(input);
}

export function bool(input) {
  return do_bool(input);
}

export function int(input) {
  return do_int(input);
}

export function float(input) {
  return do_float(input);
}

export function null$() {
  return do_null();
}

export function nullable(input, inner_type) {
  if (input instanceof Some) {
    let value = input[0];
    return inner_type(value);
  } else {
    return null$();
  }
}

export function object(entries) {
  return do_object(entries);
}

export function preprocessed_array(from) {
  return do_preprocessed_array(from);
}

export function array(entries, inner_type) {
  let _pipe = entries;
  let _pipe$1 = $list.map(_pipe, inner_type);
  return preprocessed_array(_pipe$1);
}
