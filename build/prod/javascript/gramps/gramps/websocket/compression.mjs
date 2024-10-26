import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bytes_builder from "../../../gleam_stdlib/gleam/bytes_builder.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

class Sync extends $CustomType {}

class Deflated extends $CustomType {}

class Default extends $CustomType {}

export class Compression extends $CustomType {
  constructor(inflate, deflate) {
    super();
    this.inflate = inflate;
    this.deflate = deflate;
  }
}
