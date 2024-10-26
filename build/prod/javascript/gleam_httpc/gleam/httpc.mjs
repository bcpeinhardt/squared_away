import * as $http from "../../gleam_http/gleam/http.mjs";
import * as $request from "../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../gleam_http/gleam/http/response.mjs";
import { Response } from "../../gleam_http/gleam/http/response.mjs";
import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $uri from "../../gleam_stdlib/gleam/uri.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

class Ssl extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Binary extends $CustomType {}

class BodyFormat extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Verify extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class VerifyNone extends $CustomType {}

class Builder extends $CustomType {
  constructor(verify_tls) {
    super();
    this.verify_tls = verify_tls;
  }
}

export function configure() {
  return new Builder(true);
}

export function verify_tls(_, which) {
  return new Builder(which);
}
