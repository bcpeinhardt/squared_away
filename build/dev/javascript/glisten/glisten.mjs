import * as $process from "../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../gleam_otp/gleam/otp/actor.mjs";
import * as $supervisor from "../gleam_otp/gleam/otp/supervisor.mjs";
import * as $bytes_builder from "../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";
import * as $acceptor from "./glisten/internal/acceptor.mjs";
import { Pool } from "./glisten/internal/acceptor.mjs";
import * as $handler from "./glisten/internal/handler.mjs";
import * as $listener from "./glisten/internal/listener.mjs";
import * as $socket from "./glisten/socket.mjs";
import * as $options from "./glisten/socket/options.mjs";
import { Certfile, Keyfile } from "./glisten/socket/options.mjs";
import * as $ssl from "./glisten/ssl.mjs";
import * as $transport from "./glisten/transport.mjs";

export class ListenerClosed extends $CustomType {}

export class ListenerTimeout extends $CustomType {}

export class AcceptorTimeout extends $CustomType {}

export class AcceptorFailed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class AcceptorCrashed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class SystemError extends $CustomType {
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

export class User extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class IpV4 extends $CustomType {
  constructor(x0, x1, x2, x3) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
  }
}

export class IpV6 extends $CustomType {
  constructor(x0, x1, x2, x3, x4, x5, x6, x7) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
    this[4] = x4;
    this[5] = x5;
    this[6] = x6;
    this[7] = x7;
  }
}

class Server extends $CustomType {
  constructor(listener, supervisor, transport) {
    super();
    this.listener = listener;
    this.supervisor = supervisor;
    this.transport = transport;
  }
}

export class ConnectionInfo extends $CustomType {
  constructor(port, ip_address) {
    super();
    this.port = port;
    this.ip_address = ip_address;
  }
}

export class Connection extends $CustomType {
  constructor(socket, transport, subject) {
    super();
    this.socket = socket;
    this.transport = transport;
    this.subject = subject;
  }
}

class Handler extends $CustomType {
  constructor(on_init, loop, on_close, pool_size, http2_support) {
    super();
    this.on_init = on_init;
    this.loop = loop;
    this.on_close = on_close;
    this.pool_size = pool_size;
    this.http2_support = http2_support;
  }
}

export function get_supervisor(server) {
  return server.supervisor;
}

export function convert_ip_address(ip) {
  if (ip instanceof $transport.IpV4) {
    let a = ip[0];
    let b = ip[1];
    let c = ip[2];
    let d = ip[3];
    return new IpV4(a, b, c, d);
  } else {
    let a = ip[0];
    let b = ip[1];
    let c = ip[2];
    let d = ip[3];
    let e = ip[4];
    let f = ip[5];
    let g = ip[6];
    let h = ip[7];
    return new IpV6(a, b, c, d, e, f, g, h);
  }
}

function convert_on_init(on_init) {
  return (conn) => {
    let connection = new Connection(conn.socket, conn.transport, conn.sender);
    return on_init(connection);
  };
}

export function handler(on_init, loop) {
  return new Handler(on_init, loop, new None(), 10, false);
}

export function with_close(handler, on_close) {
  return handler.withFields({ on_close: new Some(on_close) });
}

export function with_pool_size(handler, size) {
  return handler.withFields({ pool_size: size });
}

export function with_http2(handler) {
  return handler.withFields({ http2_support: true });
}
