import * as $exception from "../exception/exception.mjs";
import * as $crypto from "../gleam_crypto/gleam/crypto.mjs";
import * as $erlang from "../gleam_erlang/gleam/erlang.mjs";
import * as $atom from "../gleam_erlang/gleam/erlang/atom.mjs";
import * as $http from "../gleam_http/gleam/http.mjs";
import * as $cookie from "../gleam_http/gleam/http/cookie.mjs";
import * as $request from "../gleam_http/gleam/http/request.mjs";
import * as $response from "../gleam_http/gleam/http/response.mjs";
import { Response as HttpResponse } from "../gleam_http/gleam/http/response.mjs";
import * as $json from "../gleam_json/gleam/json.mjs";
import * as $bit_array from "../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $bytes_builder from "../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $string_builder from "../gleam_stdlib/gleam/string_builder.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import * as $logging from "../logging/logging.mjs";
import * as $marceau from "../marceau/marceau.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  makeError,
  isEqual,
  toBitArray,
  stringBits,
} from "./gleam.mjs";
import * as $internal from "./wisp/internal.mjs";

export class Text extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Bytes extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class File extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

export class Empty extends $CustomType {}

class BufferedReader extends $CustomType {
  constructor(reader, buffer) {
    super();
    this.reader = reader;
    this.buffer = buffer;
  }
}

class Quotas extends $CustomType {
  constructor(body, files) {
    super();
    this.body = body;
    this.files = files;
  }
}

export class FormData extends $CustomType {
  constructor(values, files) {
    super();
    this.values = values;
    this.files = files;
  }
}

export class UploadedFile extends $CustomType {
  constructor(file_name, path) {
    super();
    this.file_name = file_name;
    this.path = path;
  }
}

class Errored extends $CustomType {}

class Thrown extends $CustomType {}

class Exited extends $CustomType {}

export class EmergencyLevel extends $CustomType {}

export class AlertLevel extends $CustomType {}

export class CriticalLevel extends $CustomType {}

export class ErrorLevel extends $CustomType {}

export class WarningLevel extends $CustomType {}

export class NoticeLevel extends $CustomType {}

export class InfoLevel extends $CustomType {}

export class DebugLevel extends $CustomType {}

export class PlainText extends $CustomType {}

export class Signed extends $CustomType {}

export function response(status) {
  return new HttpResponse(status, toList([]), new Empty());
}

export function set_body(response, body) {
  let _pipe = response;
  return $response.set_body(_pipe, body);
}

export function file_download(response, name, path) {
  let name$1 = $uri.percent_encode(name);
  let _pipe = response;
  let _pipe$1 = $response.set_header(
    _pipe,
    "content-disposition",
    ("attachment; filename=\"" + name$1) + "\"",
  );
  return $response.set_body(_pipe$1, new File(path));
}

export function file_download_from_memory(response, name, data) {
  let name$1 = $uri.percent_encode(name);
  let _pipe = response;
  let _pipe$1 = $response.set_header(
    _pipe,
    "content-disposition",
    ("attachment; filename=\"" + name$1) + "\"",
  );
  return $response.set_body(_pipe$1, new Bytes(data));
}

export function html_response(html, status) {
  return new HttpResponse(
    status,
    toList([["content-type", "text/html; charset=utf-8"]]),
    new Text(html),
  );
}

export function json_response(json, status) {
  return new HttpResponse(
    status,
    toList([["content-type", "application/json; charset=utf-8"]]),
    new Text(json),
  );
}

export function html_body(response, html) {
  let _pipe = response;
  let _pipe$1 = $response.set_body(_pipe, new Text(html));
  return $response.set_header(
    _pipe$1,
    "content-type",
    "text/html; charset=utf-8",
  );
}

export function json_body(response, json) {
  let _pipe = response;
  let _pipe$1 = $response.set_body(_pipe, new Text(json));
  return $response.set_header(
    _pipe$1,
    "content-type",
    "application/json; charset=utf-8",
  );
}

export function string_builder_body(response, content) {
  let _pipe = response;
  return $response.set_body(_pipe, new Text(content));
}

export function string_body(response, content) {
  let _pipe = response;
  return $response.set_body(
    _pipe,
    new Text($string_builder.from_string(content)),
  );
}

export function method_not_allowed(methods) {
  let allowed = (() => {
    let _pipe = methods;
    let _pipe$1 = $list.map(_pipe, $http.method_to_string);
    let _pipe$2 = $list.sort(_pipe$1, $string.compare);
    let _pipe$3 = $string.join(_pipe$2, ", ");
    return $string.uppercase(_pipe$3);
  })();
  return new HttpResponse(405, toList([["allow", allowed]]), new Empty());
}

export function ok() {
  return new HttpResponse(200, toList([]), new Empty());
}

export function created() {
  return new HttpResponse(201, toList([]), new Empty());
}

export function accepted() {
  return new HttpResponse(202, toList([]), new Empty());
}

export function redirect(url) {
  return new HttpResponse(303, toList([["location", url]]), new Empty());
}

export function moved_permanently(url) {
  return new HttpResponse(308, toList([["location", url]]), new Empty());
}

export function no_content() {
  return new HttpResponse(204, toList([]), new Empty());
}

export function not_found() {
  return new HttpResponse(404, toList([]), new Empty());
}

export function bad_request() {
  return new HttpResponse(400, toList([]), new Empty());
}

export function entity_too_large() {
  return new HttpResponse(413, toList([]), new Empty());
}

export function unsupported_media_type(acceptable) {
  let acceptable$1 = $string.join(acceptable, ", ");
  return new HttpResponse(415, toList([["accept", acceptable$1]]), new Empty());
}

export function unprocessable_entity() {
  return new HttpResponse(422, toList([]), new Empty());
}

export function internal_server_error() {
  return new HttpResponse(500, toList([]), new Empty());
}

function decrement_body_quota(quotas, size) {
  let quotas$1 = quotas.withFields({ body: quotas.body - size });
  let $ = quotas$1.body < 0;
  if ($) {
    return new Error(entity_too_large());
  } else {
    return new Ok(quotas$1);
  }
}

function decrement_quota(quota, size) {
  let $ = quota - size;
  if ($ < 0) {
    let quota$1 = $;
    return new Error(entity_too_large());
  } else {
    let quota$1 = $;
    return new Ok(quota$1);
  }
}

function buffered_read(reader, chunk_size) {
  let $ = reader.buffer;
  if ($.length == 0) {
    return reader.reader(chunk_size);
  } else {
    return new Ok(new $internal.Chunk(reader.buffer, reader.reader));
  }
}

export function set_max_body_size(request, size) {
  let _pipe = request.body.withFields({ max_body_size: size });
  return ((_capture) => { return $request.set_body(request, _capture); })(_pipe);
}

export function get_max_body_size(request) {
  return request.body.max_body_size;
}

export function set_secret_key_base(request, key) {
  let $ = $string.byte_size(key) < 64;
  if ($) {
    throw makeError(
      "panic",
      "wisp",
      705,
      "set_secret_key_base",
      "Secret key base must be at least 64 bytes long",
      {}
    )
  } else {
    let _pipe = request.body.withFields({ secret_key_base: key });
    return ((_capture) => { return $request.set_body(request, _capture); })(
      _pipe,
    );
  }
}

export function get_secret_key_base(request) {
  return request.body.secret_key_base;
}

export function set_max_files_size(request, size) {
  let _pipe = request.body.withFields({ max_files_size: size });
  return ((_capture) => { return $request.set_body(request, _capture); })(_pipe);
}

export function get_max_files_size(request) {
  return request.body.max_files_size;
}

export function set_read_chunk_size(request, size) {
  let _pipe = request.body.withFields({ read_chunk_size: size });
  return ((_capture) => { return $request.set_body(request, _capture); })(_pipe);
}

export function get_read_chunk_size(request) {
  return request.body.read_chunk_size;
}

export function require_method(request, method, next) {
  let $ = isEqual(request.method, method);
  if ($) {
    return next();
  } else {
    return method_not_allowed(toList([method]));
  }
}

export function get_query(request) {
  let _pipe = $request.get_query(request);
  return $result.unwrap(_pipe, toList([]));
}

export function method_override(request) {
  return $bool.guard(
    !isEqual(request.method, new $http.Post()),
    request,
    () => {
      let _pipe = $result.try$(
        $request.get_query(request),
        (query) => {
          return $result.try$(
            $list.key_find(query, "_method"),
            (value) => {
              return $result.map(
                $http.parse_method(value),
                (method) => {
                  if (method instanceof $http.Put) {
                    return $request.set_method(request, method);
                  } else if (method instanceof $http.Patch) {
                    return $request.set_method(request, method);
                  } else if (method instanceof $http.Delete) {
                    return $request.set_method(request, method);
                  } else {
                    return request;
                  }
                },
              );
            },
          );
        },
      );
      return $result.unwrap(_pipe, request);
    },
  );
}

function read_body_loop(reader, read_chunk_size, max_body_size, accumulator) {
  return $result.try$(
    reader(read_chunk_size),
    (chunk) => {
      if (chunk instanceof $internal.ReadingFinished) {
        return new Ok(accumulator);
      } else {
        let chunk$1 = chunk[0];
        let next = chunk.next;
        let accumulator$1 = $bit_array.append(accumulator, chunk$1);
        let $ = $bit_array.byte_size(accumulator$1) > max_body_size;
        if ($) {
          return new Error(undefined);
        } else {
          return read_body_loop(
            next,
            read_chunk_size,
            max_body_size,
            accumulator$1,
          );
        }
      }
    },
  );
}

export function read_body_to_bitstring(request) {
  let connection = request.body;
  return read_body_loop(
    connection.reader,
    connection.read_chunk_size,
    connection.max_body_size,
    toBitArray([]),
  );
}

export function require_bit_array_body(request, next) {
  let $ = read_body_to_bitstring(request);
  if ($.isOk()) {
    let body = $[0];
    return next(body);
  } else {
    return entity_too_large();
  }
}

export function require_content_type(request, expected, next) {
  let $ = $list.key_find(request.headers, "content-type");
  if ($.isOk()) {
    let content_type = $[0];
    let $1 = $string.split_once(content_type, ";");
    if ($1.isOk() && ($1[0][0] === expected)) {
      let content_type$1 = $1[0][0];
      return next();
    } else if (content_type === expected) {
      return next();
    } else {
      return unsupported_media_type(toList([expected]));
    }
  } else {
    return unsupported_media_type(toList([expected]));
  }
}

function bit_array_to_string(bits) {
  let _pipe = $bit_array.to_string(bits);
  return $result.replace_error(_pipe, bad_request());
}

function fn_with_bad_request_error(f) {
  return (a) => {
    let _pipe = f(a);
    return $result.replace_error(_pipe, bad_request());
  };
}

function multipart_content_disposition(headers) {
  let _pipe = $result.try$(
    $list.key_find(headers, "content-disposition"),
    (header) => {
      return $result.try$(
        $http.parse_content_disposition(header),
        (header) => {
          return $result.map(
            $list.key_find(header.parameters, "name"),
            (name) => {
              let filename = $option.from_result(
                $list.key_find(header.parameters, "filename"),
              );
              return [name, filename];
            },
          );
        },
      );
    },
  );
  return $result.replace_error(_pipe, bad_request());
}

function read_chunk(reader, chunk_size) {
  let _pipe = buffered_read(reader, chunk_size);
  let _pipe$1 = $result.replace_error(_pipe, bad_request());
  return $result.try$(
    _pipe$1,
    (chunk) => {
      if (chunk instanceof $internal.Chunk) {
        let chunk$1 = chunk[0];
        let next = chunk.next;
        return new Ok([chunk$1, next]);
      } else {
        return new Error(bad_request());
      }
    },
  );
}

function multipart_body(
  reader,
  parse,
  boundary,
  chunk_size,
  quota,
  append,
  data
) {
  return $result.try$(
    read_chunk(reader, chunk_size),
    (_use0) => {
      let chunk = _use0[0];
      let reader$1 = _use0[1];
      let size_read = $bit_array.byte_size(chunk);
      return $result.try$(
        parse(chunk),
        (output) => {
          if (output instanceof $http.MultipartBody) {
            let parsed = output.chunk;
            let done = output.done;
            let remaining = output.remaining;
            let used = (size_read - $bit_array.byte_size(remaining)) - 2;
            let used$1 = (() => {
              if (done) {
                return (used - 4) - $string.byte_size(boundary);
              } else {
                return used;
              }
            })();
            return $result.try$(
              decrement_quota(quota, used$1),
              (quota) => {
                let reader$2 = new BufferedReader(reader$1, remaining);
                let reader$3 = (() => {
                  if (done) {
                    return new $option.None();
                  } else {
                    return new $option.Some(reader$2);
                  }
                })();
                return $result.map(
                  append(data, parsed),
                  (value) => { return [reader$3, quota, value]; },
                );
              },
            );
          } else {
            let chunk$1 = output.chunk;
            let parse$1 = output.continuation;
            let parse$2 = fn_with_bad_request_error(
              (_capture) => { return parse$1(_capture); },
            );
            let reader$2 = new BufferedReader(reader$1, toBitArray([]));
            return $result.try$(
              append(data, chunk$1),
              (data) => {
                return multipart_body(
                  reader$2,
                  parse$2,
                  boundary,
                  chunk_size,
                  quota,
                  append,
                  data,
                );
              },
            );
          }
        },
      );
    },
  );
}

function multipart_headers(reader, parse, chunk_size, quotas) {
  return $result.try$(
    read_chunk(reader, chunk_size),
    (_use0) => {
      let chunk = _use0[0];
      let reader$1 = _use0[1];
      return $result.try$(
        parse(chunk),
        (headers) => {
          if (headers instanceof $http.MultipartHeaders) {
            let headers$1 = headers.headers;
            let remaining = headers.remaining;
            let used = $bit_array.byte_size(chunk) - $bit_array.byte_size(
              remaining,
            );
            return $result.map(
              decrement_body_quota(quotas, used),
              (quotas) => {
                let reader$2 = new BufferedReader(reader$1, remaining);
                return [headers$1, reader$2, quotas];
              },
            );
          } else {
            let parse$1 = headers.continuation;
            let parse$2 = (chunk) => {
              let _pipe = parse$1(chunk);
              return $result.replace_error(_pipe, bad_request());
            };
            let reader$2 = new BufferedReader(reader$1, toBitArray([]));
            return multipart_headers(reader$2, parse$2, chunk_size, quotas);
          }
        },
      );
    },
  );
}

function sort_keys(pairs) {
  return $list.sort(pairs, (a, b) => { return $string.compare(a[0], b[0]); });
}

function or_400(result, next) {
  if (result.isOk()) {
    let value = result[0];
    return next(value);
  } else {
    return bad_request();
  }
}

export function require_string_body(request, next) {
  let $ = read_body_to_bitstring(request);
  if ($.isOk()) {
    let body = $[0];
    return or_400($bit_array.to_string(body), next);
  } else {
    return entity_too_large();
  }
}

export function require_json(request, next) {
  return require_content_type(
    request,
    "application/json",
    () => {
      return require_string_body(
        request,
        (body) => {
          return or_400(
            $json.decode(body, (var0) => { return new Ok(var0); }),
            (json) => { return next(json); },
          );
        },
      );
    },
  );
}

function require_urlencoded_form(request, next) {
  return require_string_body(
    request,
    (body) => {
      return or_400(
        $uri.parse_query(body),
        (pairs) => {
          let pairs$1 = sort_keys(pairs);
          return next(new FormData(pairs$1, toList([])));
        },
      );
    },
  );
}

export function serve_static(req, prefix, directory, handler) {
  let path = $internal.remove_preceeding_slashes(req.path);
  let prefix$1 = $internal.remove_preceeding_slashes(prefix);
  let $ = req.method;
  let $1 = $string.starts_with(path, prefix$1);
  if ($ instanceof $http.Get && $1) {
    let path$1 = (() => {
      let _pipe = path;
      let _pipe$1 = $string.drop_left(_pipe, $string.length(prefix$1));
      let _pipe$2 = $string.replace(_pipe$1, "..", "");
      return ((_capture) => { return $internal.join_path(directory, _capture); })(
        _pipe$2,
      );
    })();
    let mime_type = (() => {
      let _pipe = req.path;
      let _pipe$1 = $string.split(_pipe, ".");
      let _pipe$2 = $list.last(_pipe$1);
      let _pipe$3 = $result.unwrap(_pipe$2, "");
      return $marceau.extension_to_mime_type(_pipe$3);
    })();
    let content_type = (() => {
      if (mime_type === "application/json") {
        return mime_type + "; charset=utf-8";
      } else if (mime_type.startsWith("text/")) {
        return mime_type + "; charset=utf-8";
      } else {
        return mime_type;
      }
    })();
    let $2 = $simplifile.is_file(path$1);
    if ($2.isOk() && $2[0]) {
      let _pipe = $response.new$(200);
      let _pipe$1 = $response.set_header(_pipe, "content-type", content_type);
      return $response.set_body(_pipe$1, new File(path$1));
    } else {
      return handler();
    }
  } else {
    return handler();
  }
}

export function handle_head(req, handler) {
  let $ = req.method;
  if ($ instanceof $http.Head) {
    let _pipe = req;
    let _pipe$1 = $request.set_method(_pipe, new $http.Get());
    let _pipe$2 = $request.prepend_header(_pipe$1, "x-original-method", "HEAD");
    let _pipe$3 = handler(_pipe$2);
    return $response.set_body(_pipe$3, new Empty());
  } else {
    return handler(req);
  }
}

export function new_temporary_file(request) {
  let directory = request.body.temporary_directory;
  return $result.try$(
    $simplifile.create_directory_all(directory),
    (_) => {
      let path = $internal.join_path(directory, $internal.random_slug());
      return $result.map($simplifile.create_file(path), (_) => { return path; });
    },
  );
}

export function delete_temporary_files(request) {
  let $ = $simplifile.delete$(request.body.temporary_directory);
  if (!$.isOk() && $[0] instanceof $simplifile.Enoent) {
    return new Ok(undefined);
  } else {
    let other = $;
    return other;
  }
}

function log_level_to_logging_log_level(log_level) {
  if (log_level instanceof EmergencyLevel) {
    return new $logging.Emergency();
  } else if (log_level instanceof AlertLevel) {
    return new $logging.Alert();
  } else if (log_level instanceof CriticalLevel) {
    return new $logging.Critical();
  } else if (log_level instanceof ErrorLevel) {
    return new $logging.Error();
  } else if (log_level instanceof WarningLevel) {
    return new $logging.Warning();
  } else if (log_level instanceof NoticeLevel) {
    return new $logging.Notice();
  } else if (log_level instanceof InfoLevel) {
    return new $logging.Info();
  } else {
    return new $logging.Debug();
  }
}

export function random_string(length) {
  return $internal.random_string(length);
}

export function sign_message(request, message, algorithm) {
  return $crypto.sign_message(
    message,
    toBitArray([stringBits(request.body.secret_key_base)]),
    algorithm,
  );
}

export function verify_signed_message(request, message) {
  return $crypto.verify_signed_message(
    message,
    toBitArray([stringBits(request.body.secret_key_base)]),
  );
}

export function set_cookie(response, request, name, value, security, max_age) {
  let attributes = $cookie.defaults(new $http.Https()).withFields({
    max_age: new $option.Some(max_age)
  });
  let value$1 = (() => {
    if (security instanceof PlainText) {
      return $bit_array.base64_encode(toBitArray([stringBits(value)]), false);
    } else {
      return sign_message(
        request,
        toBitArray([stringBits(value)]),
        new $crypto.Sha512(),
      );
    }
  })();
  let _pipe = response;
  return $response.set_cookie(_pipe, name, value$1, attributes);
}

export function get_cookie(request, name, security) {
  return $result.try$(
    (() => {
      let _pipe = request;
      let _pipe$1 = $request.get_cookies(_pipe);
      return $list.key_find(_pipe$1, name);
    })(),
    (value) => {
      return $result.try$(
        (() => {
          if (security instanceof PlainText) {
            return $bit_array.base64_decode(value);
          } else {
            return verify_signed_message(request, value);
          }
        })(),
        (value) => { return $bit_array.to_string(value); },
      );
    },
  );
}

export function create_canned_connection(body, secret_key_base) {
  return $internal.make_connection(
    (_) => {
      return new Ok(
        new $internal.Chunk(
          body,
          (_) => { return new Ok(new $internal.ReadingFinished()); },
        ),
      );
    },
    secret_key_base,
  );
}

export const path_segments = $request.path_segments;

export const set_header = $response.set_header;

export const priv_directory = $erlang.priv_directory;
