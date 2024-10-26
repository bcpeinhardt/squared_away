import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";

class Task extends $CustomType {
  constructor(owner, pid, monitor, selector) {
    super();
    this.owner = owner;
    this.pid = pid;
    this.monitor = monitor;
    this.selector = selector;
  }
}

export class Timeout extends $CustomType {}

export class Exit extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class FromMonitor extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class FromSubject extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}
