import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";
import * as $cookie from "../../gleam/http/cookie.mjs";

export class Response extends $CustomType {
  constructor(status, headers, body) {
    super();
    this.status = status;
    this.headers = headers;
    this.body = body;
  }
}

export function new$(status) {
  return new Response(status, toList([]), "");
}

export function get_header(response, key) {
  return $list.key_find(response.headers, $string.lowercase(key));
}

export function set_header(response, key, value) {
  let headers = $list.key_set(response.headers, $string.lowercase(key), value);
  return response.withFields({ headers: headers });
}

export function prepend_header(response, key, value) {
  let headers = listPrepend([$string.lowercase(key), value], response.headers);
  return response.withFields({ headers: headers });
}

export function set_body(response, body) {
  let status = response.status;
  let headers = response.headers;
  return new Response(status, headers, body);
}

export function try_map(response, transform) {
  return $result.then$(
    transform(response.body),
    (body) => { return new Ok(set_body(response, body)); },
  );
}

export function map(response, transform) {
  let _pipe = response.body;
  let _pipe$1 = transform(_pipe);
  return ((_capture) => { return set_body(response, _capture); })(_pipe$1);
}

export function redirect(uri) {
  return new Response(
    303,
    toList([["location", uri]]),
    $string.append("You are being redirected to ", uri),
  );
}

export function get_cookies(resp) {
  let headers = resp.headers;
  let _pipe = headers;
  let _pipe$1 = $list.filter_map(
    _pipe,
    (header) => {
      let name = header[0];
      let value = header[1];
      if (name === "set-cookie") {
        return new Ok($cookie.parse(value));
      } else {
        return new Error(undefined);
      }
    },
  );
  return $list.flatten(_pipe$1);
}

export function set_cookie(response, name, value, attributes) {
  return prepend_header(
    response,
    "set-cookie",
    $cookie.set_header(name, value, attributes),
  );
}

export function expire_cookie(response, name, attributes) {
  let attrs = attributes.withFields({ max_age: new $option.Some(0) });
  return set_cookie(response, name, "", attrs);
}
