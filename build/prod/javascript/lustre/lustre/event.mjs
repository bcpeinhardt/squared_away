import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { prevent_default, stop_propagation } from "../client-runtime.ffi.mjs";
import { Ok } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import * as $effect from "../lustre/effect.mjs";

export { prevent_default, stop_propagation };

export function emit(event, data) {
  return $effect.event(event, data);
}

export function on(name, handler) {
  return $attribute.on(name, handler);
}

export function on_click(msg) {
  return on("click", (_) => { return new Ok(msg); });
}

export function on_mouse_down(msg) {
  return on("mousedown", (_) => { return new Ok(msg); });
}

export function on_mouse_up(msg) {
  return on("mouseup", (_) => { return new Ok(msg); });
}

export function on_mouse_enter(msg) {
  return on("mouseenter", (_) => { return new Ok(msg); });
}

export function on_mouse_leave(msg) {
  return on("mouseleave", (_) => { return new Ok(msg); });
}

export function on_mouse_over(msg) {
  return on("mouseover", (_) => { return new Ok(msg); });
}

export function on_mouse_out(msg) {
  return on("mouseout", (_) => { return new Ok(msg); });
}

export function on_keypress(msg) {
  return on(
    "keypress",
    (event) => {
      let _pipe = event;
      let _pipe$1 = $dynamic.field("key", $dynamic.string)(_pipe);
      return $result.map(_pipe$1, msg);
    },
  );
}

export function on_keydown(msg) {
  return on(
    "keydown",
    (event) => {
      let _pipe = event;
      let _pipe$1 = $dynamic.field("key", $dynamic.string)(_pipe);
      return $result.map(_pipe$1, msg);
    },
  );
}

export function on_keyup(msg) {
  return on(
    "keyup",
    (event) => {
      let _pipe = event;
      let _pipe$1 = $dynamic.field("key", $dynamic.string)(_pipe);
      return $result.map(_pipe$1, msg);
    },
  );
}

export function on_focus(msg) {
  return on("focus", (_) => { return new Ok(msg); });
}

export function on_blur(msg) {
  return on("blur", (_) => { return new Ok(msg); });
}

export function value(event) {
  let _pipe = event;
  return $dynamic.field("target", $dynamic.field("value", $dynamic.string))(
    _pipe,
  );
}

export function on_input(msg) {
  return on(
    "input",
    (event) => {
      let _pipe = value(event);
      return $result.map(_pipe, msg);
    },
  );
}

export function checked(event) {
  let _pipe = event;
  return $dynamic.field("target", $dynamic.field("checked", $dynamic.bool))(
    _pipe,
  );
}

export function on_check(msg) {
  return on(
    "change",
    (event) => {
      let _pipe = checked(event);
      return $result.map(_pipe, msg);
    },
  );
}

export function mouse_position(event) {
  return $result.then$(
    $dynamic.field("clientX", $dynamic.float)(event),
    (x) => {
      return $result.then$(
        $dynamic.field("clientY", $dynamic.float)(event),
        (y) => { return new Ok([x, y]); },
      );
    },
  );
}

export function on_submit(msg) {
  return on(
    "submit",
    (event) => {
      let $ = prevent_default(event);
      
      return new Ok(msg);
    },
  );
}
