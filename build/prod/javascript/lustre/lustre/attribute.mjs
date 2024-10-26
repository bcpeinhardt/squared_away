import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error } from "../gleam.mjs";
import * as $vdom from "../lustre/internals/vdom.mjs";
import { Attribute, Event } from "../lustre/internals/vdom.mjs";

export function attribute(name, value) {
  return new Attribute(name, $dynamic.from(value), false);
}

export function property(name, value) {
  return new Attribute(name, $dynamic.from(value), true);
}

export function on(name, handler) {
  return new Event("on" + name, handler);
}

export function map(attr, f) {
  if (attr instanceof Attribute) {
    let name$1 = attr[0];
    let value$1 = attr[1];
    let as_property = attr.as_property;
    return new Attribute(name$1, value$1, as_property);
  } else {
    let on$1 = attr[0];
    let handler = attr[1];
    return new Event(on$1, (e) => { return $result.map(handler(e), f); });
  }
}

export function style(properties) {
  return attribute(
    "style",
    $list.fold(
      properties,
      "",
      (styles, _use1) => {
        let name$1 = _use1[0];
        let value$1 = _use1[1];
        return (((styles + name$1) + ":") + value$1) + ";";
      },
    ),
  );
}

export function class$(name) {
  return attribute("class", name);
}

export function none() {
  return class$("");
}

export function classes(names) {
  return attribute(
    "class",
    (() => {
      let _pipe = names;
      let _pipe$1 = $list.filter_map(
        _pipe,
        (class$) => {
          let $ = class$[1];
          if ($) {
            return new Ok(class$[0]);
          } else {
            return new Error(undefined);
          }
        },
      );
      return $string.join(_pipe$1, " ");
    })(),
  );
}

export function id(name) {
  return attribute("id", name);
}

export function role(name) {
  return attribute("role", name);
}

export function title(name) {
  return attribute("title", name);
}

export function type_(name) {
  return attribute("type", name);
}

export function value(val) {
  return attribute("value", val);
}

export function checked(is_checked) {
  return property("checked", is_checked);
}

export function placeholder(text) {
  return attribute("placeholder", text);
}

export function selected(is_selected) {
  return property("selected", is_selected);
}

export function accept(types) {
  return attribute("accept", $string.join(types, ","));
}

export function accept_charset(types) {
  return attribute("acceptCharset", $string.join(types, " "));
}

export function msg(uri) {
  return attribute("msg", uri);
}

export function autocomplete(name) {
  return attribute("autocomplete", name);
}

export function autofocus(should_autofocus) {
  return property("autofocus", should_autofocus);
}

export function disabled(is_disabled) {
  return property("disabled", is_disabled);
}

export function name(name) {
  return attribute("name", name);
}

export function pattern(regex) {
  return attribute("pattern", regex);
}

export function readonly(is_readonly) {
  return property("readonly", is_readonly);
}

export function required(is_required) {
  return property("required", is_required);
}

export function for$(id) {
  return attribute("for", id);
}

export function max(val) {
  return attribute("max", val);
}

export function min(val) {
  return attribute("min", val);
}

export function step(val) {
  return attribute("step", val);
}

export function cols(val) {
  return attribute("cols", $int.to_string(val));
}

export function rows(val) {
  return attribute("rows", $int.to_string(val));
}

export function wrap(mode) {
  return attribute("wrap", mode);
}

export function href(uri) {
  return attribute("href", uri);
}

export function target(target) {
  return attribute("target", target);
}

export function download(filename) {
  return attribute("download", filename);
}

export function rel(relationship) {
  return attribute("rel", relationship);
}

export function src(uri) {
  return attribute("src", uri);
}

export function height(val) {
  return property("height", val);
}

export function width(val) {
  return property("width", val);
}

export function alt(text) {
  return attribute("alt", text);
}

export function autoplay(should_autoplay) {
  return property("autoplay", should_autoplay);
}

export function controls(visible) {
  return property("controls", visible);
}

export function loop(should_loop) {
  return property("loop", should_loop);
}

export function action(url) {
  return attribute("action", url);
}

export function enctype(value) {
  return attribute("enctype", value);
}

export function method(method) {
  return attribute("method", method);
}

export function novalidate(value) {
  return property("novalidate", value);
}

export function form_action(action) {
  return attribute("formaction", action);
}

export function form_enctype(value) {
  return attribute("formenctype", value);
}

export function form_method(method) {
  return attribute("formmethod", method);
}

export function form_novalidate(value) {
  return property("formnovalidate", value);
}

export function form_target(target) {
  return attribute("formtarget", target);
}

export function open(is_open) {
  return property("open", is_open);
}
