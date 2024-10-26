import * as $atom from "../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../gleam_erlang/gleam/erlang/process.mjs";
import * as $bytes_builder from "../../gleam_stdlib/gleam/bytes_builder.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $socket from "../glisten/socket.mjs";
import * as $options from "../glisten/socket/options.mjs";
import * as $ssl from "../glisten/ssl.mjs";
import * as $tcp from "../glisten/tcp.mjs";

export class Tcp extends $CustomType {}

export class Ssl extends $CustomType {}

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

function decode_ipv4() {
  return $dynamic.decode4(
    (var0, var1, var2, var3) => { return new IpV4(var0, var1, var2, var3); },
    $dynamic.element(0, $dynamic.int),
    $dynamic.element(1, $dynamic.int),
    $dynamic.element(2, $dynamic.int),
    $dynamic.element(3, $dynamic.int),
  );
}
