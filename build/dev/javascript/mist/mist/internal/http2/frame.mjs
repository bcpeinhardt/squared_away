import * as $bit_array from "../../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $logging from "../../../../logging/logging.mjs";
import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  makeError,
  toBitArray,
  sizedInt,
} from "../../../gleam.mjs";

class StreamIdentifier extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class HeaderPriority extends $CustomType {
  constructor(exclusive, stream_dependency, weight) {
    super();
    this.exclusive = exclusive;
    this.stream_dependency = stream_dependency;
    this.weight = weight;
  }
}

export class Complete extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Continued extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Enabled extends $CustomType {}

export class Disabled extends $CustomType {}

export class HeaderTableSize extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class ServerPush extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MaxConcurrentStreams extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class InitialWindowSize extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MaxFrameSize extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MaxHeaderListSize extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Data extends $CustomType {
  constructor(data, end_stream, identifier) {
    super();
    this.data = data;
    this.end_stream = end_stream;
    this.identifier = identifier;
  }
}

export class Header extends $CustomType {
  constructor(data, end_stream, identifier, priority) {
    super();
    this.data = data;
    this.end_stream = end_stream;
    this.identifier = identifier;
    this.priority = priority;
  }
}

export class Priority extends $CustomType {
  constructor(exclusive, identifier, stream_dependency, weight) {
    super();
    this.exclusive = exclusive;
    this.identifier = identifier;
    this.stream_dependency = stream_dependency;
    this.weight = weight;
  }
}

export class Termination extends $CustomType {
  constructor(error, identifier) {
    super();
    this.error = error;
    this.identifier = identifier;
  }
}

export class Settings extends $CustomType {
  constructor(ack, settings) {
    super();
    this.ack = ack;
    this.settings = settings;
  }
}

export class PushPromise extends $CustomType {
  constructor(data, identifier, promised_stream_id) {
    super();
    this.data = data;
    this.identifier = identifier;
    this.promised_stream_id = promised_stream_id;
  }
}

export class Ping extends $CustomType {
  constructor(ack, data) {
    super();
    this.ack = ack;
    this.data = data;
  }
}

export class GoAway extends $CustomType {
  constructor(data, error, last_stream_id) {
    super();
    this.data = data;
    this.error = error;
    this.last_stream_id = last_stream_id;
  }
}

export class WindowUpdate extends $CustomType {
  constructor(amount, identifier) {
    super();
    this.amount = amount;
    this.identifier = identifier;
  }
}

export class Continuation extends $CustomType {
  constructor(data, identifier) {
    super();
    this.data = data;
    this.identifier = identifier;
  }
}

export class NoError extends $CustomType {}

export class ProtocolError extends $CustomType {}

export class InternalError extends $CustomType {}

export class FlowControlError extends $CustomType {}

export class SettingsTimeout extends $CustomType {}

export class StreamClosed extends $CustomType {}

export class FrameSizeError extends $CustomType {}

export class RefusedStream extends $CustomType {}

export class Cancel extends $CustomType {}

export class CompressionError extends $CustomType {}

export class ConnectError extends $CustomType {}

export class EnhanceYourCalm extends $CustomType {}

export class InadequateSecurity extends $CustomType {}

export class Http11Required extends $CustomType {}

export class Unsupported extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function stream_identifier(value) {
  return new StreamIdentifier(value);
}

export function get_stream_identifier(identifier) {
  let value = identifier[0];
  return value;
}

function parse_priority(identifier, flags, length, payload) {
  let $ = toBitArray([flags.buffer, payload.buffer]);
  if (length === 5 && $.length == 5 && (identifier !== 0)) {
    let exclusive = $.intFromSlice(1, 1, true, false);
    let dependency = $.intFromSlice(1, 4, true, false);
    let weight = $.byteAt(4);
    return new Ok(
      new Priority(
        exclusive === 1,
        stream_identifier(identifier),
        stream_identifier(dependency),
        weight,
      ),
    );
  } else if (length === 5) {
    return new Error(new ProtocolError());
  } else {
    return new Error(new FrameSizeError());
  }
}

function parse_window_update(identifier, flags, length, payload) {
  let $ = toBitArray([flags.buffer, payload.buffer]);
  if (length === 4 && $.length == 4 && ($.intFromSlice(1, 4, true, false) !== 0)) {
    let window_size = $.intFromSlice(1, 4, true, false);
    return new Ok(new WindowUpdate(window_size, stream_identifier(identifier)));
  } else if (length === 4) {
    return new Error(new FrameSizeError());
  } else {
    return new Error(new ProtocolError());
  }
}

function get_error(value) {
  if (value === 0) {
    return new NoError();
  } else if (value === 1) {
    return new ProtocolError();
  } else if (value === 2) {
    return new InternalError();
  } else if (value === 3) {
    return new FlowControlError();
  } else if (value === 4) {
    return new SettingsTimeout();
  } else if (value === 5) {
    return new StreamClosed();
  } else if (value === 6) {
    return new FrameSizeError();
  } else if (value === 7) {
    return new RefusedStream();
  } else if (value === 8) {
    return new Cancel();
  } else if (value === 9) {
    return new CompressionError();
  } else if (value === 10) {
    return new ConnectError();
  } else if (value === 11) {
    return new EnhanceYourCalm();
  } else if (value === 12) {
    return new InadequateSecurity();
  } else if (value === 13) {
    return new Http11Required();
  } else {
    let n = value;
    return new Unsupported(n);
  }
}

function parse_termination(identifier, flags, length, payload) {
  let $ = toBitArray([flags.buffer, payload.buffer]);
  if (length === 4 && $.length == 5 && (identifier !== 0)) {
    let error = $.intFromSlice(1, 5, true, false);
    return new Ok(
      new Termination(get_error(error), stream_identifier(identifier)),
    );
  } else if (length === 4) {
    return new Error(new ProtocolError());
  } else {
    return new Error(new FrameSizeError());
  }
}

function get_setting(identifier, value) {
  if (identifier === 1) {
    return new Ok(new HeaderTableSize(value));
  } else if (identifier === 2) {
    return new Ok(
      new ServerPush(
        (() => {
          if (value === 0) {
            return new Disabled();
          } else if (value === 1) {
            return new Enabled();
          } else {
            throw makeError(
              "panic",
              "mist/internal/http2/frame",
              638,
              "get_setting",
              "Somehow a bit was neither 0 nor 1",
              {}
            )
          }
        })(),
      ),
    );
  } else if (identifier === 3) {
    return new Ok(new MaxConcurrentStreams(value));
  } else if (identifier === 4) {
    if (value > 2_147_483_647) {
      let n = value;
      return new Error(new FlowControlError());
    } else {
      return new Ok(new InitialWindowSize(value));
    }
  } else if (identifier === 5) {
    if (value > 16_777_215) {
      let n = value;
      return new Error(new ProtocolError());
    } else {
      return new Ok(new MaxFrameSize(value));
    }
  } else if (identifier === 6) {
    return new Ok(new MaxHeaderListSize(value));
  } else {
    return new Error(new ProtocolError());
  }
}

function from_bool(bool) {
  if (bool) {
    return 1;
  } else {
    return 0;
  }
}

function encode_data(data) {
  if (data instanceof Complete) {
    let data$1 = data[0];
    return [1, data$1];
  } else {
    let data$1 = data[0];
    return [0, data$1];
  }
}

function encode_error(error) {
  if (error instanceof NoError) {
    return 0;
  } else if (error instanceof ProtocolError) {
    return 1;
  } else if (error instanceof InternalError) {
    return 2;
  } else if (error instanceof FlowControlError) {
    return 3;
  } else if (error instanceof SettingsTimeout) {
    return 4;
  } else if (error instanceof StreamClosed) {
    return 5;
  } else if (error instanceof FrameSizeError) {
    return 6;
  } else if (error instanceof RefusedStream) {
    return 7;
  } else if (error instanceof Cancel) {
    return 8;
  } else if (error instanceof CompressionError) {
    return 9;
  } else if (error instanceof ConnectError) {
    return 10;
  } else if (error instanceof EnhanceYourCalm) {
    return 11;
  } else if (error instanceof InadequateSecurity) {
    return 12;
  } else if (error instanceof Http11Required) {
    return 13;
  } else {
    return 69;
  }
}

function encode_settings(settings) {
  return $list.fold(
    settings,
    toBitArray([]),
    (acc, setting) => {
      if (setting instanceof HeaderTableSize) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(1, 16, true), sizedInt(value, 32, true)]),
        );
      } else if (setting instanceof ServerPush && setting[0] instanceof Enabled) {
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(2, 16, true), sizedInt(1, 32, true)]),
        );
      } else if (setting instanceof ServerPush && setting[0] instanceof Disabled) {
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(2, 16, true), sizedInt(0, 32, true)]),
        );
      } else if (setting instanceof MaxConcurrentStreams) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(3, 16, true), sizedInt(value, 32, true)]),
        );
      } else if (setting instanceof InitialWindowSize) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(4, 16, true), sizedInt(value, 32, true)]),
        );
      } else if (setting instanceof MaxFrameSize) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(5, 16, true), sizedInt(value, 32, true)]),
        );
      } else {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([sizedInt(6, 16, true), sizedInt(value, 32, true)]),
        );
      }
    },
  );
}

export function settings_ack() {
  return new Settings(true, toList([]));
}
