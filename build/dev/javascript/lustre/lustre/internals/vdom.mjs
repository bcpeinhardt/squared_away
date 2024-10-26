import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $string_builder from "../../../gleam_stdlib/gleam/string_builder.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../../gleam.mjs";
import { coerce as unsafe_coerce } from "../../lustre-escape.ffi.mjs";
import * as $escape from "../../lustre/internals/escape.mjs";
import { escape } from "../../lustre/internals/escape.mjs";

export class Text extends $CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
}

export class Element extends $CustomType {
  constructor(key, namespace, tag, attrs, children, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children;
    this.self_closing = self_closing;
    this.void = void$;
  }
}

export class Map extends $CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
}

export class Fragment extends $CustomType {
  constructor(elements, key) {
    super();
    this.elements = elements;
    this.key = key;
  }
}

export class Attribute extends $CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
}

export class Event extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export function attribute_to_event_handler(attribute) {
  if (attribute instanceof Attribute) {
    return new Error(undefined);
  } else {
    let name = attribute[0];
    let handler = attribute[1];
    let name$1 = $string.drop_left(name, 2);
    return new Ok([name$1, handler]);
  }
}

export function attribute_to_json(attribute, key) {
  let true_atom = $dynamic.from(true);
  let false_atom = $dynamic.from(false);
  if (attribute instanceof Attribute && attribute.as_property) {
    return new Error(undefined);
  } else if (attribute instanceof Attribute && !attribute.as_property) {
    let name = attribute[0];
    let value = attribute[1];
    let $ = $dynamic.classify(value);
    if ($ === "String") {
      return new Ok(
        $json.object(
          toList([
            ["0", $json.string(name)],
            ["1", $json.string(unsafe_coerce(value))],
          ]),
        ),
      );
    } else if ($ === "Atom" &&
    ((isEqual(value, true_atom)) || (isEqual(value, false_atom)))) {
      return new Ok(
        $json.object(
          toList([
            ["0", $json.string(name)],
            ["1", $json.bool(unsafe_coerce(value))],
          ]),
        ),
      );
    } else if ($ === "Bool") {
      return new Ok(
        $json.object(
          toList([
            ["0", $json.string(name)],
            ["1", $json.bool(unsafe_coerce(value))],
          ]),
        ),
      );
    } else if ($ === "Boolean") {
      return new Ok(
        $json.object(
          toList([
            ["0", $json.string(name)],
            ["1", $json.bool(unsafe_coerce(value))],
          ]),
        ),
      );
    } else if ($ === "Int") {
      return new Ok(
        $json.object(
          toList([
            ["0", $json.string(name)],
            ["1", $json.int(unsafe_coerce(value))],
          ]),
        ),
      );
    } else if ($ === "Float") {
      return new Ok(
        $json.object(
          toList([
            ["0", $json.string(name)],
            ["1", $json.float(unsafe_coerce(value))],
          ]),
        ),
      );
    } else {
      return new Error(undefined);
    }
  } else {
    let name = attribute[0];
    let name$1 = $string.drop_left(name, 2);
    return new Ok(
      $json.object(
        toList([
          ["0", $json.string("data-lustre-on-" + name$1)],
          ["1", $json.string((key + "-") + name$1)],
        ]),
      ),
    );
  }
}

function attribute_to_string_parts(attr) {
  if (attr instanceof Attribute && attr[0] === "") {
    return new Error(undefined);
  } else if (attr instanceof Attribute) {
    let name = attr[0];
    let value = attr[1];
    let as_property = attr.as_property;
    let true_atom = $dynamic.from(true);
    let $ = $dynamic.classify(value);
    if ($ === "String") {
      return new Ok([name, unsafe_coerce(value)]);
    } else if ($ === "Atom" && (isEqual(value, true_atom))) {
      return new Ok([name, ""]);
    } else if ($ === "Bool" && (isEqual(value, true_atom))) {
      return new Ok([name, ""]);
    } else if ($ === "Boolean" && (isEqual(value, true_atom))) {
      return new Ok([name, ""]);
    } else if ($ === "Atom") {
      return new Error(undefined);
    } else if ($ === "Bool") {
      return new Error(undefined);
    } else if ($ === "Boolean") {
      return new Error(undefined);
    } else if ($ === "Int") {
      return new Ok([name, $int.to_string(unsafe_coerce(value))]);
    } else if ($ === "Float") {
      return new Ok([name, $float.to_string(unsafe_coerce(value))]);
    } else if (as_property) {
      return new Error(undefined);
    } else {
      return new Ok([name, $string.inspect(value)]);
    }
  } else {
    return new Error(undefined);
  }
}

function attributes_to_string_builder(attrs) {
  let $ = (() => {
    let init = [$string_builder.new$(), "", "", ""];
    return $list.fold(
      attrs,
      init,
      (_use0, attr) => {
        let html = _use0[0];
        let class$ = _use0[1];
        let style = _use0[2];
        let inner_html = _use0[3];
        let $1 = attribute_to_string_parts(attr);
        if ($1.isOk() && $1[0][0] === "dangerous-unescaped-html") {
          let val = $1[0][1];
          return [html, class$, style, inner_html + val];
        } else if ($1.isOk() && $1[0][0] === "class" && (class$ === "")) {
          let val = $1[0][1];
          return [html, escape(val), style, inner_html];
        } else if ($1.isOk() && $1[0][0] === "class") {
          let val = $1[0][1];
          return [html, (class$ + " ") + escape(val), style, inner_html];
        } else if ($1.isOk() && $1[0][0] === "style" && (style === "")) {
          let val = $1[0][1];
          return [html, class$, escape(val), inner_html];
        } else if ($1.isOk() && $1[0][0] === "style") {
          let val = $1[0][1];
          return [html, class$, (style + " ") + escape(val), inner_html];
        } else if ($1.isOk() && $1[0][1] === "") {
          let key = $1[0][0];
          return [
            $string_builder.append(html, " " + key),
            class$,
            style,
            inner_html,
          ];
        } else if ($1.isOk()) {
          let key = $1[0][0];
          let val = $1[0][1];
          return [
            $string_builder.append(
              html,
              (((" " + key) + "=\"") + escape(val)) + "\"",
            ),
            class$,
            style,
            inner_html,
          ];
        } else {
          return [html, class$, style, inner_html];
        }
      },
    );
  })();
  let html = $[0];
  let class$ = $[1];
  let style = $[2];
  let inner_html = $[3];
  return [
    (() => {
      if (class$ === "" && style === "") {
        return html;
      } else if (style === "") {
        return $string_builder.append(html, (" class=\"" + class$) + "\"");
      } else if (class$ === "") {
        return $string_builder.append(html, (" style=\"" + style) + "\"");
      } else {
        return $string_builder.append(
          html,
          (((" class=\"" + class$) + "\" style=\"") + style) + "\"",
        );
      }
    })(),
    inner_html,
  ];
}

function children_to_string_builder(html, children, raw_text) {
  return $list.fold(
    children,
    html,
    (html, child) => {
      let _pipe = child;
      let _pipe$1 = do_element_to_string_builder(_pipe, raw_text);
      return ((_capture) => {
        return $string_builder.append_builder(html, _capture);
      })(_pipe$1);
    },
  );
}

function do_element_to_string_builder(loop$element, loop$raw_text) {
  while (true) {
    let element = loop$element;
    let raw_text = loop$raw_text;
    if (element instanceof Text && element.content === "") {
      return $string_builder.new$();
    } else if (element instanceof Text && (raw_text)) {
      let content = element.content;
      return $string_builder.from_string(content);
    } else if (element instanceof Text) {
      let content = element.content;
      return $string_builder.from_string(escape(content));
    } else if (element instanceof Map) {
      let subtree = element.subtree;
      loop$element = subtree();
      loop$raw_text = raw_text;
    } else if (element instanceof Element && (element.self_closing)) {
      let namespace = element.namespace;
      let tag = element.tag;
      let attrs = element.attrs;
      let self_closing = element.self_closing;
      let html = $string_builder.from_string("<" + tag);
      let $ = attributes_to_string_builder(
        (() => {
          if (namespace === "") {
            return attrs;
          } else {
            return listPrepend(
              new Attribute("xmlns", $dynamic.from(namespace), false),
              attrs,
            );
          }
        })(),
      );
      let attrs$1 = $[0];
      let _pipe = html;
      let _pipe$1 = $string_builder.append_builder(_pipe, attrs$1);
      return $string_builder.append(_pipe$1, "/>");
    } else if (element instanceof Element && (element.void)) {
      let namespace = element.namespace;
      let tag = element.tag;
      let attrs = element.attrs;
      let void$ = element.void;
      let html = $string_builder.from_string("<" + tag);
      let $ = attributes_to_string_builder(
        (() => {
          if (namespace === "") {
            return attrs;
          } else {
            return listPrepend(
              new Attribute("xmlns", $dynamic.from(namespace), false),
              attrs,
            );
          }
        })(),
      );
      let attrs$1 = $[0];
      let _pipe = html;
      let _pipe$1 = $string_builder.append_builder(_pipe, attrs$1);
      return $string_builder.append(_pipe$1, ">");
    } else if (element instanceof Element &&
    element.namespace === "" &&
    element.tag === "style" &&
    !element.self_closing &&
    !element.void) {
      let tag = element.tag;
      let attrs = element.attrs;
      let children = element.children;
      let html = $string_builder.from_string("<" + tag);
      let $ = attributes_to_string_builder(attrs);
      let attrs$1 = $[0];
      let _pipe = html;
      let _pipe$1 = $string_builder.append_builder(_pipe, attrs$1);
      let _pipe$2 = $string_builder.append(_pipe$1, ">");
      let _pipe$3 = children_to_string_builder(_pipe$2, children, true);
      return $string_builder.append(_pipe$3, ("</" + tag) + ">");
    } else if (element instanceof Element &&
    element.namespace === "" &&
    element.tag === "script" &&
    !element.self_closing &&
    !element.void) {
      let tag = element.tag;
      let attrs = element.attrs;
      let children = element.children;
      let html = $string_builder.from_string("<" + tag);
      let $ = attributes_to_string_builder(attrs);
      let attrs$1 = $[0];
      let _pipe = html;
      let _pipe$1 = $string_builder.append_builder(_pipe, attrs$1);
      let _pipe$2 = $string_builder.append(_pipe$1, ">");
      let _pipe$3 = children_to_string_builder(_pipe$2, children, true);
      return $string_builder.append(_pipe$3, ("</" + tag) + ">");
    } else if (element instanceof Element) {
      let namespace = element.namespace;
      let tag = element.tag;
      let attrs = element.attrs;
      let children = element.children;
      let html = $string_builder.from_string("<" + tag);
      let $ = attributes_to_string_builder(
        (() => {
          if (namespace === "") {
            return attrs;
          } else {
            return listPrepend(
              new Attribute("xmlns", $dynamic.from(namespace), false),
              attrs,
            );
          }
        })(),
      );
      let attrs$1 = $[0];
      let inner_html = $[1];
      if (inner_html === "") {
        let _pipe = html;
        let _pipe$1 = $string_builder.append_builder(_pipe, attrs$1);
        let _pipe$2 = $string_builder.append(_pipe$1, ">");
        let _pipe$3 = children_to_string_builder(_pipe$2, children, raw_text);
        return $string_builder.append(_pipe$3, ("</" + tag) + ">");
      } else {
        let _pipe = html;
        let _pipe$1 = $string_builder.append_builder(_pipe, attrs$1);
        return $string_builder.append(
          _pipe$1,
          (((">" + inner_html) + "</") + tag) + ">",
        );
      }
    } else {
      let elements = element.elements;
      return children_to_string_builder(
        $string_builder.new$(),
        elements,
        raw_text,
      );
    }
  }
}

export function element_to_string(element) {
  let _pipe = element;
  let _pipe$1 = do_element_to_string_builder(_pipe, false);
  return $string_builder.to_string(_pipe$1);
}

export function element_to_string_builder(element) {
  return do_element_to_string_builder(element, false);
}

function do_element_list_to_json(elements, key) {
  return $json.preprocessed_array(
    $list.index_map(
      elements,
      (element, index) => {
        let key$1 = (key + "-") + $int.to_string(index);
        return element_to_json(element, key$1);
      },
    ),
  );
}

export function element_to_json(loop$element, loop$key) {
  while (true) {
    let element = loop$element;
    let key = loop$key;
    if (element instanceof Text) {
      let content = element.content;
      return $json.object(toList([["content", $json.string(content)]]));
    } else if (element instanceof Map) {
      let subtree = element.subtree;
      loop$element = subtree();
      loop$key = key;
    } else if (element instanceof Element) {
      let namespace = element.namespace;
      let tag = element.tag;
      let attrs = element.attrs;
      let children = element.children;
      let self_closing = element.self_closing;
      let void$ = element.void;
      let attrs$1 = $json.preprocessed_array(
        $list.filter_map(
          attrs,
          (_capture) => { return attribute_to_json(_capture, key); },
        ),
      );
      let children$1 = do_element_list_to_json(children, key);
      return $json.object(
        toList([
          ["namespace", $json.string(namespace)],
          ["tag", $json.string(tag)],
          ["attrs", attrs$1],
          ["children", children$1],
          ["self_closing", $json.bool(self_closing)],
          ["void", $json.bool(void$)],
        ]),
      );
    } else {
      let elements = element.elements;
      return $json.object(
        toList([["elements", do_element_list_to_json(elements, key)]]),
      );
    }
  }
}

function do_element_list_handlers(elements, handlers, key) {
  return $list.index_fold(
    elements,
    handlers,
    (handlers, element, index) => {
      let key$1 = (key + "-") + $int.to_string(index);
      return do_handlers(element, handlers, key$1);
    },
  );
}

function do_handlers(loop$element, loop$handlers, loop$key) {
  while (true) {
    let element = loop$element;
    let handlers = loop$handlers;
    let key = loop$key;
    if (element instanceof Text) {
      return handlers;
    } else if (element instanceof Map) {
      let subtree = element.subtree;
      loop$element = subtree();
      loop$handlers = handlers;
      loop$key = key;
    } else if (element instanceof Element) {
      let attrs = element.attrs;
      let children = element.children;
      let handlers$1 = $list.fold(
        attrs,
        handlers,
        (handlers, attr) => {
          let $ = attribute_to_event_handler(attr);
          if ($.isOk()) {
            let name = $[0][0];
            let handler = $[0][1];
            return $dict.insert(handlers, (key + "-") + name, handler);
          } else {
            return handlers;
          }
        },
      );
      return do_element_list_handlers(children, handlers$1, key);
    } else {
      let elements = element.elements;
      return do_element_list_handlers(elements, handlers, key);
    }
  }
}

export function handlers(element) {
  return do_handlers(element, $dict.new$(), "0");
}
