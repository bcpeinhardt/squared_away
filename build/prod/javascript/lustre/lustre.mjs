import * as $process from "../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../gleam_otp/gleam/otp/actor.mjs";
import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import { register } from "./client-component.ffi.mjs";
import { start as do_start, is_browser, is_registered } from "./client-runtime.ffi.mjs";
import { Error, CustomType as $CustomType } from "./gleam.mjs";
import * as $effect from "./lustre/effect.mjs";
import * as $element from "./lustre/element.mjs";
import * as $patch from "./lustre/internals/patch.mjs";
import * as $runtime from "./lustre/internals/runtime.mjs";
import { start as start_server_component } from "./server-runtime.ffi.mjs";

export { is_browser, is_registered, register, start_server_component };

class App extends $CustomType {
  constructor(init, update, view, on_attribute_change) {
    super();
    this.init = init;
    this.update = update;
    this.view = view;
    this.on_attribute_change = on_attribute_change;
  }
}

export class ActorError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class BadComponentName extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ComponentAlreadyRegistered extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class ElementNotFound extends $CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
}

export class NotABrowser extends $CustomType {}

export class NotErlang extends $CustomType {}

export function application(init, update, view) {
  return new App(init, update, view, new None());
}

export function element(html) {
  let init = (_) => { return [undefined, $effect.none()]; };
  let update = (_, _1) => { return [undefined, $effect.none()]; };
  let view = (_) => { return html; };
  return application(init, update, view);
}

export function simple(init, update, view) {
  let init$1 = (flags) => { return [init(flags), $effect.none()]; };
  let update$1 = (model, msg) => { return [update(model, msg), $effect.none()]; };
  return application(init$1, update$1, view);
}

export function component(init, update, view, on_attribute_change) {
  return new App(init, update, view, new Some(on_attribute_change));
}

function do_start_actor(_, _1) {
  return new Error(new NotErlang());
}

export function start_actor(app, flags) {
  return do_start_actor(app, flags);
}

export function dispatch(msg) {
  return new $runtime.Dispatch(msg);
}

export function shutdown() {
  return new $runtime.Shutdown();
}

export function start(app, selector, flags) {
  return $bool.guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => { return do_start(app, selector, flags); },
  );
}
