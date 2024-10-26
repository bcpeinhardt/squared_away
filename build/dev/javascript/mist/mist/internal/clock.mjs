import * as $birl from "../../../birl/birl.mjs";
import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $logging from "../../../logging/logging.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

export class SetTime extends $CustomType {}

class MistClock extends $CustomType {}

class DateHeader extends $CustomType {}

export class Set extends $CustomType {}

export class Protected extends $CustomType {}

export class NamedTable extends $CustomType {}

export class ReadConcurrency extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function date() {
  let _pipe = $birl.now();
  return $birl.to_http(_pipe);
}
