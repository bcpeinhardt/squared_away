import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $regex from "../../../gleam_stdlib/gleam/regex.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
} from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";

export class Lax extends $CustomType {}

export class Strict extends $CustomType {}

export class None extends $CustomType {}

export class Attributes extends $CustomType {
  constructor(max_age, domain, path, secure, http_only, same_site) {
    super();
    this.max_age = max_age;
    this.domain = domain;
    this.path = path;
    this.secure = secure;
    this.http_only = http_only;
    this.same_site = same_site;
  }
}

function same_site_to_string(policy) {
  if (policy instanceof Lax) {
    return "Lax";
  } else if (policy instanceof Strict) {
    return "Strict";
  } else {
    return "None";
  }
}

export function defaults(scheme) {
  return new Attributes(
    new $option.None(),
    new $option.None(),
    new $option.Some("/"),
    isEqual(scheme, new $http.Https()),
    true,
    new Some(new Lax()),
  );
}

function check_token(loop$token) {
  while (true) {
    let token = loop$token;
    let $ = $string.pop_grapheme(token);
    if (!$.isOk() && !$[0]) {
      return new Ok(undefined);
    } else if ($.isOk() && $[0][0] === " ") {
      return new Error(undefined);
    } else if ($.isOk() && $[0][0] === "\t") {
      return new Error(undefined);
    } else if ($.isOk() && $[0][0] === "\r") {
      return new Error(undefined);
    } else if ($.isOk() && $[0][0] === "\n") {
      return new Error(undefined);
    } else if ($.isOk() && $[0][0] === "\f") {
      return new Error(undefined);
    } else {
      let rest = $[0][1];
      loop$token = rest;
    }
  }
}

export function parse(cookie_string) {
  let $ = $regex.from_string("[,;]");
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "gleam/http/cookie",
      101,
      "parse",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let re = $[0];
  let _pipe = $regex.split(re, cookie_string);
  return $list.filter_map(
    _pipe,
    (pair) => {
      let $1 = $string.split_once($string.trim(pair), "=");
      if ($1.isOk() && $1[0][0] === "") {
        return new Error(undefined);
      } else if ($1.isOk()) {
        let key = $1[0][0];
        let value = $1[0][1];
        let key$1 = $string.trim(key);
        let value$1 = $string.trim(value);
        return $result.then$(
          check_token(key$1),
          (_) => {
            return $result.then$(
              check_token(value$1),
              (_) => { return new Ok([key$1, value$1]); },
            );
          },
        );
      } else {
        return new Error(undefined);
      }
    },
  );
}

const epoch = "Expires=Thu, 01 Jan 1970 00:00:00 GMT";

function cookie_attributes_to_list(attributes) {
  let max_age = attributes.max_age;
  let domain = attributes.domain;
  let path = attributes.path;
  let secure = attributes.secure;
  let http_only = attributes.http_only;
  let same_site = attributes.same_site;
  let _pipe = toList([
    (() => {
      if (max_age instanceof $option.Some && max_age[0] === 0) {
        return new $option.Some(toList([epoch]));
      } else {
        return new $option.None();
      }
    })(),
    $option.map(
      max_age,
      (max_age) => { return toList(["Max-Age=", $int.to_string(max_age)]); },
    ),
    $option.map(domain, (domain) => { return toList(["Domain=", domain]); }),
    $option.map(path, (path) => { return toList(["Path=", path]); }),
    (() => {
      if (secure) {
        return new $option.Some(toList(["Secure"]));
      } else {
        return new $option.None();
      }
    })(),
    (() => {
      if (http_only) {
        return new $option.Some(toList(["HttpOnly"]));
      } else {
        return new $option.None();
      }
    })(),
    $option.map(
      same_site,
      (same_site) => {
        return toList(["SameSite=", same_site_to_string(same_site)]);
      },
    ),
  ]);
  return $list.filter_map(
    _pipe,
    (_capture) => { return $option.to_result(_capture, undefined); },
  );
}

export function set_header(name, value, attributes) {
  let _pipe = listPrepend(
    toList([name, "=", value]),
    cookie_attributes_to_list(attributes),
  );
  let _pipe$1 = $list.map(
    _pipe,
    (_capture) => { return $string.join(_capture, ""); },
  );
  return $string.join(_pipe$1, "; ");
}
