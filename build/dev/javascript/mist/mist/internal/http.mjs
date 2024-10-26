import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $charlist from "../../../gleam_erlang/gleam/erlang/charlist.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $http from "../../../gleam_http/gleam/http.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import * as $response from "../../../gleam_http/gleam/http/response.mjs";
import { Response } from "../../../gleam_http/gleam/http/response.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bytes_builder from "../../../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $iterator from "../../../gleam_stdlib/gleam/iterator.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $pair from "../../../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $glisten from "../../../glisten/glisten.mjs";
import * as $transport from "../../../glisten/glisten/transport.mjs";
import * as $websocket from "../../../gramps/gramps/websocket.mjs";
import { CustomType as $CustomType, makeError, toBitArray } from "../../gleam.mjs";
import * as $buffer from "../../mist/internal/buffer.mjs";
import { Buffer } from "../../mist/internal/buffer.mjs";
import * as $clock from "../../mist/internal/clock.mjs";
import * as $encoder from "../../mist/internal/encoder.mjs";
import * as $file from "../../mist/internal/file.mjs";

export class Websocket extends $CustomType {
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

export class Chunked extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class File extends $CustomType {
  constructor(descriptor, offset, length) {
    super();
    this.descriptor = descriptor;
    this.offset = offset;
    this.length = length;
  }
}

export class ServerSentEvents extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Connection extends $CustomType {
  constructor(body, socket, transport) {
    super();
    this.body = body;
    this.socket = socket;
    this.transport = transport;
  }
}

export class Http extends $CustomType {}

export class HttphBin extends $CustomType {}

export class HttpBin extends $CustomType {}

export class AbsPath extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class HttpRequest extends $CustomType {
  constructor(x0, x1, x2) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
  }
}

export class HttpHeader extends $CustomType {
  constructor(x0, x1, x2, x3) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
  }
}

export class BinaryData extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class EndOfHeaders extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MoreData extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Http2Upgrade extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MalformedRequest extends $CustomType {}

export class InvalidMethod extends $CustomType {}

export class InvalidPath extends $CustomType {}

export class UnknownHeader extends $CustomType {}

export class UnknownMethod extends $CustomType {}

export class InvalidBody extends $CustomType {}

export class DiscardPacket extends $CustomType {}

export class NoHostHeader extends $CustomType {}

export class InvalidHttpVersion extends $CustomType {}

export class Chunk extends $CustomType {
  constructor(data, buffer) {
    super();
    this.data = data;
    this.buffer = buffer;
  }
}

export class Complete extends $CustomType {}

export class Http1 extends $CustomType {}

export class Http11 extends $CustomType {}

export class Http1Request extends $CustomType {
  constructor(request, version) {
    super();
    this.request = request;
    this.version = version;
  }
}

export class Upgrade extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Initial extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Stream extends $CustomType {
  constructor(selector, data, remaining, attempts) {
    super();
    this.selector = selector;
    this.data = data;
    this.remaining = remaining;
    this.attempts = attempts;
  }
}

export class Sha extends $CustomType {}

export function from_header(value) {
  let $ = $bit_array.to_string(value);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "mist/internal/http",
      77,
      "from_header",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let value$1 = $[0];
  return $string.lowercase(value$1);
}

export function version_to_string(version) {
  if (version instanceof Http1) {
    return "1.0";
  } else {
    return "1.1";
  }
}

export function connection_close(resp) {
  return $response.set_header(resp, "connection", "close");
}

export function keep_alive(resp) {
  return $response.set_header(resp, "connection", "keep-alive");
}

export function maybe_keep_alive(resp) {
  let $ = $response.get_header(resp, "connection");
  if ($.isOk()) {
    return resp;
  } else {
    return $response.set_header(resp, "connection", "keep-alive");
  }
}

export function add_content_length(when, length) {
  return (resp) => {
    if (when) {
      let $ = (() => {
        let _pipe = resp.headers;
        let _pipe$1 = $list.key_pop(_pipe, "content-length");
        return $result.lazy_unwrap(
          _pipe$1,
          () => { return ["", resp.headers]; },
        );
      })();
      let headers = $[1];
      let _pipe = resp.withFields({ headers: headers });
      return $response.set_header(
        _pipe,
        "content-length",
        $int.to_string(length),
      );
    } else {
      return resp;
    }
  };
}

function is_continue(req) {
  let _pipe = req.headers;
  let _pipe$1 = $list.find(
    _pipe,
    (tup) => {
      return ($pair.first(tup) === "expect") && ($pair.second(tup) === "100-continue");
    },
  );
  return $result.is_ok(_pipe$1);
}

const crnl = /* @__PURE__ */ toBitArray([13, 10]);

const websocket_key = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
