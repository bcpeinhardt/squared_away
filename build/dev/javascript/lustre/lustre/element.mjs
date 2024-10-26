import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $string_builder from "../../gleam_stdlib/gleam/string_builder.mjs";
import { toList, prepend as listPrepend } from "../gleam.mjs";
import * as $attribute from "../lustre/attribute.mjs";
import { attribute } from "../lustre/attribute.mjs";
import * as $vdom from "../lustre/internals/vdom.mjs";
import { Element, Fragment, Map, Text } from "../lustre/internals/vdom.mjs";

export function element(tag, attrs, children) {
  if (tag === "area") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element("", "", tag, attrs, children, false, false);
  }
}

function do_keyed(el, key) {
  if (el instanceof Element) {
    let namespace = el.namespace;
    let tag = el.tag;
    let attrs = el.attrs;
    let children = el.children;
    let self_closing = el.self_closing;
    let void$ = el.void;
    return new Element(
      key,
      namespace,
      tag,
      attrs,
      children,
      self_closing,
      void$,
    );
  } else if (el instanceof Map) {
    let subtree = el.subtree;
    return new Map(() => { return do_keyed(subtree(), key); });
  } else if (el instanceof Fragment) {
    let elements = el.elements;
    let _pipe = elements;
    let _pipe$1 = $list.index_map(
      _pipe,
      (element, idx) => {
        if (element instanceof Element) {
          let el_key = element.key;
          let new_key = (() => {
            if (el_key === "") {
              return (key + "-") + $int.to_string(idx);
            } else {
              return (key + "-") + el_key;
            }
          })();
          return do_keyed(element, new_key);
        } else {
          return do_keyed(element, key);
        }
      },
    );
    return new Fragment(_pipe$1, key);
  } else {
    return el;
  }
}

export function keyed(el, children) {
  return el(
    $list.map(
      children,
      (_use0) => {
        let key = _use0[0];
        let child = _use0[1];
        return do_keyed(child, key);
      },
    ),
  );
}

export function namespaced(namespace, tag, attrs, children) {
  return new Element("", namespace, tag, attrs, children, false, false);
}

export function advanced(namespace, tag, attrs, children, self_closing, void$) {
  return new Element("", namespace, tag, attrs, children, self_closing, void$);
}

export function text(content) {
  return new Text(content);
}

export function none() {
  return new Text("");
}

function flatten_fragment_elements(elements) {
  return $list.fold_right(
    elements,
    toList([]),
    (new_elements, element) => {
      if (element instanceof Fragment) {
        let fr_elements = element.elements;
        return $list.append(fr_elements, new_elements);
      } else {
        let el = element;
        return listPrepend(el, new_elements);
      }
    },
  );
}

export function fragment(elements) {
  let _pipe = flatten_fragment_elements(elements);
  return new Fragment(_pipe, "");
}

export function map(element, f) {
  if (element instanceof Text) {
    let content = element.content;
    return new Text(content);
  } else if (element instanceof Map) {
    let subtree = element.subtree;
    return new Map(() => { return map(subtree(), f); });
  } else if (element instanceof Element) {
    let key = element.key;
    let namespace = element.namespace;
    let tag = element.tag;
    let attrs = element.attrs;
    let children = element.children;
    let self_closing = element.self_closing;
    let void$ = element.void;
    return new Map(
      () => {
        return new Element(
          key,
          namespace,
          tag,
          $list.map(
            attrs,
            (_capture) => { return $attribute.map(_capture, f); },
          ),
          $list.map(children, (_capture) => { return map(_capture, f); }),
          self_closing,
          void$,
        );
      },
    );
  } else {
    let elements = element.elements;
    let key = element.key;
    return new Map(
      () => {
        return new Fragment(
          $list.map(elements, (_capture) => { return map(_capture, f); }),
          key,
        );
      },
    );
  }
}

export function to_string(element) {
  return $vdom.element_to_string(element);
}

export function to_document_string(el) {
  let _pipe = $vdom.element_to_string(
    (() => {
      if (el instanceof Element && el.tag === "html") {
        return el;
      } else if (el instanceof Element && el.tag === "head") {
        return element("html", toList([]), toList([el]));
      } else if (el instanceof Element && el.tag === "body") {
        return element("html", toList([]), toList([el]));
      } else if (el instanceof Map) {
        let subtree = el.subtree;
        return subtree();
      } else {
        return element(
          "html",
          toList([]),
          toList([element("body", toList([]), toList([el]))]),
        );
      }
    })(),
  );
  return ((_capture) => { return $string.append("<!doctype html>\n", _capture); })(
    _pipe,
  );
}

export function to_string_builder(element) {
  return $vdom.element_to_string_builder(element);
}

export function to_document_string_builder(el) {
  let _pipe = $vdom.element_to_string_builder(
    (() => {
      if (el instanceof Element && el.tag === "html") {
        return el;
      } else if (el instanceof Element && el.tag === "head") {
        return element("html", toList([]), toList([el]));
      } else if (el instanceof Element && el.tag === "body") {
        return element("html", toList([]), toList([el]));
      } else if (el instanceof Map) {
        let subtree = el.subtree;
        return subtree();
      } else {
        return element(
          "html",
          toList([]),
          toList([element("body", toList([]), toList([el]))]),
        );
      }
    })(),
  );
  return $string_builder.prepend(_pipe, "<!doctype html>\n");
}
