import * as $atom from "../../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";

export class OneForOne extends $CustomType {}

export class OneForAll extends $CustomType {}

export class RestForOne extends $CustomType {}

export class Never extends $CustomType {}

export class AnySignificant extends $CustomType {}

export class AllSignificant extends $CustomType {}

class Builder extends $CustomType {
  constructor(strategy, intensity, period, auto_shutdown, children) {
    super();
    this.strategy = strategy;
    this.intensity = intensity;
    this.period = period;
    this.auto_shutdown = auto_shutdown;
    this.children = children;
  }
}

export class Permanent extends $CustomType {}

export class Transient extends $CustomType {}

export class Temporary extends $CustomType {}

export class Worker extends $CustomType {
  constructor(shutdown_ms) {
    super();
    this.shutdown_ms = shutdown_ms;
  }
}

export class Supervisor extends $CustomType {}

class ChildBuilder extends $CustomType {
  constructor(id, starter, restart, significant, child_type) {
    super();
    this.id = id;
    this.starter = starter;
    this.restart = restart;
    this.significant = significant;
    this.child_type = child_type;
  }
}

export function new$(strategy) {
  return new Builder(strategy, 2, 5, new Never(), toList([]));
}

export function restart_tolerance(builder, intensity, period) {
  return builder.withFields({ intensity: intensity, period: period });
}

export function auto_shutdown(builder, value) {
  return builder.withFields({ auto_shutdown: value });
}

export function add(builder, child) {
  return builder.withFields({ children: listPrepend(child, builder.children) });
}

export function worker_child(id, starter) {
  return new ChildBuilder(
    id,
    () => {
      let _pipe = starter();
      return $result.map_error(_pipe, $dynamic.from);
    },
    new Permanent(),
    false,
    new Worker(5000),
  );
}

export function supervisor_child(id, starter) {
  return new ChildBuilder(
    id,
    () => {
      let _pipe = starter();
      return $result.map_error(_pipe, $dynamic.from);
    },
    new Permanent(),
    false,
    new Supervisor(),
  );
}

export function significant(child, significant) {
  return child.withFields({ significant: significant });
}

export function timeout(child, ms) {
  let $ = child.child_type;
  if ($ instanceof Worker) {
    return child.withFields({ child_type: new Worker(ms) });
  } else {
    return child;
  }
}

export function restart(child, restart) {
  return child.withFields({ restart: restart });
}

export function init(start_data) {
  return new Ok(start_data);
}
