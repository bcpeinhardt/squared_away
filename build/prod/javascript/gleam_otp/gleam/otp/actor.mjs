import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $charlist from "../../../gleam_erlang/gleam/erlang/charlist.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import { Abnormal } from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";
import * as $system from "../../gleam/otp/system.mjs";
import { GetState, GetStatus, Resume, Running, StatusInfo, Suspend, Suspended } from "../../gleam/otp/system.mjs";

class Message extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class System extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Unexpected extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Continue extends $CustomType {
  constructor(state, selector) {
    super();
    this.state = state;
    this.selector = selector;
  }
}

export class Stop extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Ready extends $CustomType {
  constructor(state, selector) {
    super();
    this.state = state;
    this.selector = selector;
  }
}

export class Failed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Self extends $CustomType {
  constructor(mode, parent, state, subject, selector, debug_state, message_handler) {
    super();
    this.mode = mode;
    this.parent = parent;
    this.state = state;
    this.subject = subject;
    this.selector = selector;
    this.debug_state = debug_state;
    this.message_handler = message_handler;
  }
}

export class Spec extends $CustomType {
  constructor(init, init_timeout, loop) {
    super();
    this.init = init;
    this.init_timeout = init_timeout;
    this.loop = loop;
  }
}

export class InitTimeout extends $CustomType {}

export class InitFailed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class InitCrashed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Ack extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Mon extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function continue$(state) {
  return new Continue(state, new None());
}

export function with_selector(value, selector) {
  if (value instanceof Continue) {
    let state = value.state;
    return new Continue(state, new Some(selector));
  } else {
    return value;
  }
}

function exit_process(reason) {
  return reason;
}

export function to_erlang_start_result(res) {
  if (res.isOk()) {
    let x = res[0];
    return new Ok($process.subject_owner(x));
  } else {
    let x = res[0];
    return new Error($dynamic.from(x));
  }
}
