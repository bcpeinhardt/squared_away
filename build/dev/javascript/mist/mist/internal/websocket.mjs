import * as $erlang from "../../../gleam_erlang/gleam/erlang.mjs";
import { rescue } from "../../../gleam_erlang/gleam/erlang.mjs";
import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $glisten from "../../../glisten/glisten.mjs";
import * as $options from "../../../glisten/glisten/socket/options.mjs";
import * as $transport from "../../../glisten/glisten/transport.mjs";
import * as $websocket from "../../../gramps/gramps/websocket.mjs";
import { CloseFrame, Control, InvalidFrame, NeedMoreData, PingFrame, PongFrame } from "../../../gramps/gramps/websocket.mjs";
import * as $compression from "../../../gramps/gramps/websocket/compression.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export class SocketMessage extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class SocketClosedMessage extends $CustomType {}

export class UserMessage extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Valid extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Invalid extends $CustomType {}

export class WebsocketConnection extends $CustomType {
  constructor(socket, transport, deflate) {
    super();
    this.socket = socket;
    this.transport = transport;
    this.deflate = deflate;
  }
}

export class Internal extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class User extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class WebsocketState extends $CustomType {
  constructor(buffer, user, permessage_deflate) {
    super();
    this.buffer = buffer;
    this.user = user;
    this.permessage_deflate = permessage_deflate;
  }
}
