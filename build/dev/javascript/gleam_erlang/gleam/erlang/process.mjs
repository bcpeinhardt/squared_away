import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $erlang from "../../gleam/erlang.mjs";
import * as $atom from "../../gleam/erlang/atom.mjs";

class Subject extends $CustomType {
  constructor(owner, tag) {
    super();
    this.owner = owner;
    this.tag = tag;
  }
}

export class ExitMessage extends $CustomType {
  constructor(pid, reason) {
    super();
    this.pid = pid;
    this.reason = reason;
  }
}

export class Normal extends $CustomType {}

export class Killed extends $CustomType {}

export class Abnormal extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class Anything extends $CustomType {}

class Process extends $CustomType {}

class ProcessMonitor extends $CustomType {
  constructor(tag) {
    super();
    this.tag = tag;
  }
}

export class ProcessDown extends $CustomType {
  constructor(pid, reason) {
    super();
    this.pid = pid;
    this.reason = reason;
  }
}

export class CalleeDown extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CallTimeout extends $CustomType {}

export class TimerNotFound extends $CustomType {}

export class Cancelled extends $CustomType {
  constructor(time_remaining) {
    super();
    this.time_remaining = time_remaining;
  }
}

class Kill extends $CustomType {}

export function subject_owner(subject) {
  return subject.owner;
}
