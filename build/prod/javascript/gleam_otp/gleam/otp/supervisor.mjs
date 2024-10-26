import * as $node from "../../../gleam_erlang/gleam/erlang/node.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, CustomType as $CustomType } from "../../gleam.mjs";
import * as $actor from "../../gleam/otp/actor.mjs";
import * as $intensity_tracker from "../../gleam/otp/intensity_tracker.mjs";

export class Spec extends $CustomType {
  constructor(argument, max_frequency, frequency_period, init) {
    super();
    this.argument = argument;
    this.max_frequency = max_frequency;
    this.frequency_period = frequency_period;
    this.init = init;
  }
}

class Ready extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Failed extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class ChildSpec extends $CustomType {
  constructor(start, returning) {
    super();
    this.start = start;
    this.returning = returning;
  }
}

class ChildStartError extends $CustomType {
  constructor(previous_pid, error) {
    super();
    this.previous_pid = previous_pid;
    this.error = error;
  }
}

class Exit extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class RetryRestart extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class StartAll extends $CustomType {}

class StartFrom extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class State extends $CustomType {
  constructor(restarts, starter, retry_restarts) {
    super();
    this.restarts = restarts;
    this.starter = starter;
    this.retry_restarts = retry_restarts;
  }
}

class Starter extends $CustomType {
  constructor(argument, exec) {
    super();
    this.argument = argument;
    this.exec = exec;
  }
}

class Child extends $CustomType {
  constructor(pid, argument) {
    super();
    this.pid = pid;
    this.argument = argument;
  }
}

class RestartFailed extends $CustomType {
  constructor(pid, restarts) {
    super();
    this.pid = pid;
    this.restarts = restarts;
  }
}

class TooManyRestarts extends $CustomType {}

export class Normal extends $CustomType {}

export class Takeover extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Failover extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

function start_child(child_spec, argument) {
  return $result.then$(
    (() => {
      let _pipe = child_spec.start(argument);
      return $result.map_error(
        _pipe,
        (_capture) => { return new ChildStartError(new None(), _capture); },
      );
    })(),
    (subject) => {
      return new Ok(
        new Child(
          $process.subject_owner(subject),
          child_spec.returning(argument, subject),
        ),
      );
    },
  );
}

export function supervisor(start) {
  return new ChildSpec(start, (argument, _) => { return argument; });
}

export function worker(start) {
  return new ChildSpec(start, (argument, _) => { return argument; });
}

export function returning(child, updater) {
  return new ChildSpec(child.start, updater);
}

export function to_erlang_start_result(res) {
  return $actor.to_erlang_start_result(res);
}
