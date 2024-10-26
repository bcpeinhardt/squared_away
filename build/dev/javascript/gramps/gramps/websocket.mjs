import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $bytes_builder from "../../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  prepend as listPrepend,
  CustomType as $CustomType,
  toBitArray,
} from "../gleam.mjs";
import * as $compression from "../gramps/websocket/compression.mjs";

export class TextFrame extends $CustomType {
  constructor(payload_length, payload) {
    super();
    this.payload_length = payload_length;
    this.payload = payload;
  }
}

export class BinaryFrame extends $CustomType {
  constructor(payload_length, payload) {
    super();
    this.payload_length = payload_length;
    this.payload = payload;
  }
}

export class CloseFrame extends $CustomType {
  constructor(payload_length, payload) {
    super();
    this.payload_length = payload_length;
    this.payload = payload;
  }
}

export class PingFrame extends $CustomType {
  constructor(payload_length, payload) {
    super();
    this.payload_length = payload_length;
    this.payload = payload;
  }
}

export class PongFrame extends $CustomType {
  constructor(payload_length, payload) {
    super();
    this.payload_length = payload_length;
    this.payload = payload;
  }
}

export class Data extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Control extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Continuation extends $CustomType {
  constructor(length, payload) {
    super();
    this.length = length;
    this.payload = payload;
  }
}

export class NeedMoreData extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class InvalidFrame extends $CustomType {}

export class Complete extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Incomplete extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Sha extends $CustomType {}

export function frame_to_bytes_builder(frame, mask) {
  if (frame instanceof Data && frame[0] instanceof TextFrame) {
    let payload_length = frame[0].payload_length;
    let payload = frame[0].payload;
    return make_frame(1, payload_length, payload, mask);
  } else if (frame instanceof Control && frame[0] instanceof CloseFrame) {
    let payload_length = frame[0].payload_length;
    let payload = frame[0].payload;
    return make_frame(8, payload_length, payload, mask);
  } else if (frame instanceof Data && frame[0] instanceof BinaryFrame) {
    let payload_length = frame[0].payload_length;
    let payload = frame[0].payload;
    return make_frame(2, payload_length, payload, mask);
  } else if (frame instanceof Control && frame[0] instanceof PongFrame) {
    let payload_length = frame[0].payload_length;
    let payload = frame[0].payload;
    return make_frame(10, payload_length, payload, mask);
  } else if (frame instanceof Control && frame[0] instanceof PingFrame) {
    let payload_length = frame[0].payload_length;
    let payload = frame[0].payload;
    return make_frame(9, payload_length, payload, mask);
  } else {
    let length = frame.length;
    let payload = frame.payload;
    return make_frame(0, length, payload, mask);
  }
}

function append_frame(left, length, data) {
  if (left instanceof Data && left[0] instanceof TextFrame) {
    let len = left[0].payload_length;
    let payload = left[0].payload;
    return new Data(
      new TextFrame(len + length, toBitArray([payload.buffer, data.buffer])),
    );
  } else if (left instanceof Data && left[0] instanceof BinaryFrame) {
    let len = left[0].payload_length;
    let payload = left[0].payload;
    return new Data(
      new BinaryFrame(len + length, toBitArray([payload.buffer, data.buffer])),
    );
  } else if (left instanceof Control && left[0] instanceof CloseFrame) {
    let len = left[0].payload_length;
    let payload = left[0].payload;
    return new Control(
      new CloseFrame(len + length, toBitArray([payload.buffer, data.buffer])),
    );
  } else if (left instanceof Control && left[0] instanceof PingFrame) {
    let len = left[0].payload_length;
    let payload = left[0].payload;
    return new Control(
      new PingFrame(len + length, toBitArray([payload.buffer, data.buffer])),
    );
  } else if (left instanceof Control && left[0] instanceof PongFrame) {
    let len = left[0].payload_length;
    let payload = left[0].payload;
    return new Control(
      new PongFrame(len + length, toBitArray([payload.buffer, data.buffer])),
    );
  } else {
    return left;
  }
}

export function aggregate_frames(loop$frames, loop$previous, loop$joined) {
  while (true) {
    let frames = loop$frames;
    let previous = loop$previous;
    let joined = loop$joined;
    if (frames.hasLength(0)) {
      return new Ok($list.reverse(joined));
    } else if (frames.atLeastLength(1) &&
    frames.head instanceof Complete &&
    frames.head[0] instanceof Continuation &&
    previous instanceof Some) {
      let length = frames.head[0].length;
      let data = frames.head[0].payload;
      let rest = frames.tail;
      let prev = previous[0];
      let next = append_frame(prev, length, data);
      loop$frames = rest;
      loop$previous = new None();
      loop$joined = listPrepend(next, joined);
    } else if (frames.atLeastLength(1) &&
    frames.head instanceof Incomplete &&
    frames.head[0] instanceof Continuation &&
    previous instanceof Some) {
      let length = frames.head[0].length;
      let data = frames.head[0].payload;
      let rest = frames.tail;
      let prev = previous[0];
      let next = append_frame(prev, length, data);
      loop$frames = rest;
      loop$previous = new Some(next);
      loop$joined = joined;
    } else if (frames.atLeastLength(1) &&
    frames.head instanceof Incomplete &&
    previous instanceof None) {
      let frame = frames.head[0];
      let rest = frames.tail;
      loop$frames = rest;
      loop$previous = new Some(frame);
      loop$joined = joined;
    } else if (frames.atLeastLength(1) &&
    frames.head instanceof Complete &&
    previous instanceof None) {
      let frame = frames.head[0];
      let rest = frames.tail;
      loop$frames = rest;
      loop$previous = new None();
      loop$joined = listPrepend(frame, joined);
    } else {
      return new Error(undefined);
    }
  }
}

export function has_deflate(extensions) {
  return $list.any(
    extensions,
    (str) => { return str === "permessage-deflate"; },
  );
}

const websocket_key = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

export const client_key = "dGhlIHNhbXBsZSBub25jZQ==";
