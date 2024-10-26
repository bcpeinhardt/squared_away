import * as $erlang from "../../../gleam_erlang/gleam/erlang.mjs";
import { rescue } from "../../../gleam_erlang/gleam/erlang.mjs";
import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $socket from "../../glisten/socket.mjs";
import * as $options from "../../glisten/socket/options.mjs";
import * as $transport from "../../glisten/transport.mjs";

export class Close extends $CustomType {}

export class Ready extends $CustomType {}

export class ReceiveMessage extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class SslClosed extends $CustomType {}

export class TcpClosed extends $CustomType {}

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

export class Packet extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Custom extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class LoopState extends $CustomType {
  constructor(client_ip, socket, sender, transport, data) {
    super();
    this.client_ip = client_ip;
    this.socket = socket;
    this.sender = sender;
    this.transport = transport;
    this.data = data;
  }
}

export class Connection extends $CustomType {
  constructor(client_ip, socket, transport, sender) {
    super();
    this.client_ip = client_ip;
    this.socket = socket;
    this.transport = transport;
    this.sender = sender;
  }
}

export class Handler extends $CustomType {
  constructor(socket, loop, on_init, on_close, transport) {
    super();
    this.socket = socket;
    this.loop = loop;
    this.on_init = on_init;
    this.on_close = on_close;
    this.transport = transport;
  }
}
