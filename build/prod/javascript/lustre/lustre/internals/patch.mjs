import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../../../gleam_stdlib/gleam/order.mjs";
import * as $set from "../../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  isEqual,
} from "../../gleam.mjs";
import { coerce as unsafe_coerce } from "../../lustre-escape.ffi.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";
import * as $vdom from "../../lustre/internals/vdom.mjs";
import { Attribute, Element, Event, Fragment, Map, Text } from "../../lustre/internals/vdom.mjs";

export class Diff extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Emit extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Init extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class ElementDiff extends $CustomType {
  constructor(created, removed, updated, handlers) {
    super();
    this.created = created;
    this.removed = removed;
    this.updated = updated;
    this.handlers = handlers;
  }
}

export class AttributeDiff extends $CustomType {
  constructor(created, removed, handlers) {
    super();
    this.created = created;
    this.removed = removed;
    this.handlers = handlers;
  }
}

function do_attribute(diff, key, old, new$) {
  if (!old.isOk() && !new$.isOk()) {
    return diff;
  } else if (old.isOk() &&
  new$.isOk() &&
  new$[0] instanceof Event &&
  (isEqual(old[0], new$[0]))) {
    let old$1 = old[0];
    let new$1 = new$[0];
    let name = new$[0][0];
    let handler = new$[0][1];
    return diff.withFields({
      handlers: $dict.insert(diff.handlers, name, handler)
    });
  } else if (old.isOk() && new$.isOk() && (isEqual(old[0], new$[0]))) {
    let old$1 = old[0];
    let new$1 = new$[0];
    return diff;
  } else if (old.isOk() && !new$.isOk()) {
    return diff.withFields({ removed: $set.insert(diff.removed, key) });
  } else if (new$.isOk() && new$[0] instanceof Event) {
    let new$1 = new$[0];
    let name = new$[0][0];
    let handler = new$[0][1];
    return diff.withFields({
      created: $set.insert(diff.created, new$1),
      handlers: $dict.insert(diff.handlers, name, handler)
    });
  } else {
    let new$1 = new$[0];
    return diff.withFields({ created: $set.insert(diff.created, new$1) });
  }
}

function do_key_sort(loop$xs, loop$ys) {
  while (true) {
    let xs = loop$xs;
    let ys = loop$ys;
    if (xs.hasLength(0) && ys.hasLength(0)) {
      return new Eq();
    } else if (xs.hasLength(0)) {
      return new Lt();
    } else if (ys.hasLength(0)) {
      return new Gt();
    } else if (xs.atLeastLength(1) &&
    xs.head === "-" &&
    ys.atLeastLength(1) &&
    ys.head === "-") {
      let xs$1 = xs.tail;
      let ys$1 = ys.tail;
      loop$xs = xs$1;
      loop$ys = ys$1;
    } else {
      let x = xs.head;
      let xs$1 = xs.tail;
      let y = ys.head;
      let ys$1 = ys.tail;
      let $ = $int.parse(x);
      if (!$.isOk()) {
        throw makeError(
          "assignment_no_match",
          "lustre/internals/patch",
          298,
          "do_key_sort",
          "Assignment pattern did not match",
          { value: $ }
        )
      }
      let x$1 = $[0];
      let $1 = $int.parse(y);
      if (!$1.isOk()) {
        throw makeError(
          "assignment_no_match",
          "lustre/internals/patch",
          299,
          "do_key_sort",
          "Assignment pattern did not match",
          { value: $1 }
        )
      }
      let y$1 = $1[0];
      let $2 = $int.compare(x$1, y$1);
      if ($2 instanceof Eq) {
        loop$xs = xs$1;
        loop$ys = ys$1;
      } else {
        let order = $2;
        return order;
      }
    }
  }
}

function key_sort(x, y) {
  return do_key_sort($string.split(x, "-"), $string.split(y, "-"));
}

export function attribute_diff_to_json(diff, key) {
  return $json.preprocessed_array(
    toList([
      $json.preprocessed_array(
        $set.fold(
          diff.created,
          toList([]),
          (array, attr) => {
            let $ = $vdom.attribute_to_json(attr, key);
            if ($.isOk()) {
              let json = $[0];
              return listPrepend(json, array);
            } else {
              return array;
            }
          },
        ),
      ),
      $json.preprocessed_array(
        $set.fold(
          diff.removed,
          toList([]),
          (array, key) => { return listPrepend($json.string(key), array); },
        ),
      ),
    ]),
  );
}

function zip(xs, ys) {
  if (xs.hasLength(0) && ys.hasLength(0)) {
    return toList([]);
  } else if (xs.atLeastLength(1) && ys.atLeastLength(1)) {
    let x = xs.head;
    let xs$1 = xs.tail;
    let y = ys.head;
    let ys$1 = ys.tail;
    return listPrepend([new Some(x), new Some(y)], zip(xs$1, ys$1));
  } else if (xs.atLeastLength(1) && ys.hasLength(0)) {
    let x = xs.head;
    let xs$1 = xs.tail;
    return listPrepend([new Some(x), new None()], zip(xs$1, toList([])));
  } else {
    let y = ys.head;
    let ys$1 = ys.tail;
    return listPrepend([new None(), new Some(y)], zip(toList([]), ys$1));
  }
}

function event_handler(attribute) {
  if (attribute instanceof Attribute) {
    return new Error(undefined);
  } else {
    let name = attribute[0];
    let handler = attribute[1];
    let name$1 = $string.drop_left(name, 2);
    return new Ok([name$1, handler]);
  }
}

export function is_empty_element_diff(diff) {
  return ((isEqual(diff.created, $dict.new$())) && (isEqual(
    diff.removed,
    $set.new$()
  ))) && (isEqual(diff.updated, $dict.new$()));
}

function is_empty_attribute_diff(diff) {
  return (isEqual(diff.created, $set.new$())) && (isEqual(
    diff.removed,
    $set.new$()
  ));
}

export function element_diff_to_json(diff) {
  return $json.preprocessed_array(
    toList([
      $json.preprocessed_array(
        $list.reverse(
          (() => {
            let _pipe = $dict.to_list(diff.created);
            let _pipe$1 = $list.sort(
              _pipe,
              (x, y) => { return key_sort(x[0], y[0]); },
            );
            return $list.fold(
              _pipe$1,
              toList([]),
              (array, patch) => {
                let key = patch[0];
                let element = patch[1];
                let json = $json.preprocessed_array(
                  toList([
                    $json.string(key),
                    $vdom.element_to_json(element, key),
                  ]),
                );
                return listPrepend(json, array);
              },
            );
          })(),
        ),
      ),
      $json.preprocessed_array(
        (() => {
          let _pipe = $set.to_list(diff.removed);
          let _pipe$1 = $list.sort(_pipe, key_sort);
          return $list.fold(
            _pipe$1,
            toList([]),
            (array, key) => {
              let json = $json.preprocessed_array(toList([$json.string(key)]));
              return listPrepend(json, array);
            },
          );
        })(),
      ),
      $json.preprocessed_array(
        $list.reverse(
          $dict.fold(
            diff.updated,
            toList([]),
            (array, key, diff) => {
              return $bool.guard(
                is_empty_attribute_diff(diff),
                array,
                () => {
                  let json = $json.preprocessed_array(
                    toList([
                      $json.string(key),
                      attribute_diff_to_json(diff, key),
                    ]),
                  );
                  return listPrepend(json, array);
                },
              );
            },
          ),
        ),
      ),
    ]),
  );
}

export function patch_to_json(patch) {
  if (patch instanceof Diff) {
    let diff = patch[0];
    return $json.preprocessed_array(
      toList([$json.int($constants.diff), element_diff_to_json(diff)]),
    );
  } else if (patch instanceof Emit) {
    let name = patch[0];
    let event = patch[1];
    return $json.preprocessed_array(
      toList([$json.int($constants.emit), $json.string(name), event]),
    );
  } else {
    let attrs = patch[0];
    let element = patch[1];
    return $json.preprocessed_array(
      toList([
        $json.int($constants.init),
        $json.array(attrs, $json.string),
        $vdom.element_to_json(element, "0"),
      ]),
    );
  }
}

function attribute_dict(attributes) {
  return $list.fold(
    attributes,
    $dict.new$(),
    (dict, attr) => {
      if (attr instanceof Attribute && attr[0] === "class") {
        let value = attr[1];
        let $ = $dict.get(dict, "class");
        if ($.isOk() && $[0] instanceof Attribute) {
          let classes = $[0][1];
          let classes$1 = $dynamic.from(
            (unsafe_coerce(classes) + " ") + unsafe_coerce(value),
          );
          return $dict.insert(
            dict,
            "class",
            new Attribute("class", classes$1, false),
          );
        } else if ($.isOk()) {
          return $dict.insert(dict, "class", attr);
        } else {
          return $dict.insert(dict, "class", attr);
        }
      } else if (attr instanceof Attribute && attr[0] === "style") {
        let value = attr[1];
        let $ = $dict.get(dict, "style");
        if ($.isOk() && $[0] instanceof Attribute) {
          let styles = $[0][1];
          let styles$1 = $dynamic.from(
            $list.append(unsafe_coerce(styles), unsafe_coerce(value)),
          );
          return $dict.insert(
            dict,
            "style",
            new Attribute("style", styles$1, false),
          );
        } else if ($.isOk()) {
          return $dict.insert(dict, "class", attr);
        } else {
          return $dict.insert(dict, "class", attr);
        }
      } else if (attr instanceof Attribute) {
        let key = attr[0];
        return $dict.insert(dict, key, attr);
      } else {
        let key = attr[0];
        return $dict.insert(dict, key, attr);
      }
    },
  );
}

export function attributes(old, new$) {
  let old$1 = attribute_dict(old);
  let new$1 = attribute_dict(new$);
  let init = new AttributeDiff($set.new$(), $set.new$(), $dict.new$());
  let $ = $dict.fold(
    old$1,
    [init, new$1],
    (_use0, key, attr) => {
      let diff = _use0[0];
      let new$2 = _use0[1];
      let new_attr = $dict.get(new$2, key);
      let diff$1 = do_attribute(diff, key, new Ok(attr), new_attr);
      let new$3 = $dict.delete$(new$2, key);
      return [diff$1, new$3];
    },
  );
  let diff = $[0];
  let new$2 = $[1];
  return $dict.fold(
    new$2,
    diff,
    (diff, key, attr) => {
      return do_attribute(diff, key, new Error(undefined), new Ok(attr));
    },
  );
}

function do_element_list(diff, old_elements, new_elements, key) {
  let children = zip(old_elements, new_elements);
  return $list.index_fold(
    children,
    diff,
    (diff, _use1, pos) => {
      let old = _use1[0];
      let new$ = _use1[1];
      let key$1 = (key + "-") + $int.to_string(pos);
      return do_elements(diff, old, new$, key$1);
    },
  );
}

function do_elements(loop$diff, loop$old, loop$new, loop$key) {
  while (true) {
    let diff = loop$diff;
    let old = loop$old;
    let new$ = loop$new;
    let key = loop$key;
    if (old instanceof None && new$ instanceof None) {
      return diff;
    } else if (old instanceof Some && new$ instanceof None) {
      return diff.withFields({ removed: $set.insert(diff.removed, key) });
    } else if (old instanceof None && new$ instanceof Some) {
      let new$1 = new$[0];
      return diff.withFields({
        created: $dict.insert(diff.created, key, new$1),
        handlers: fold_event_handlers(diff.handlers, new$1, key)
      });
    } else {
      let old$1 = old[0];
      let new$1 = new$[0];
      if (old$1 instanceof Map && new$1 instanceof Map) {
        let old_subtree = old$1.subtree;
        let new_subtree = new$1.subtree;
        loop$diff = diff;
        loop$old = new Some(old_subtree());
        loop$new = new Some(new_subtree());
        loop$key = key;
      } else if (old$1 instanceof Map) {
        let subtree = old$1.subtree;
        loop$diff = diff;
        loop$old = new Some(subtree());
        loop$new = new Some(new$1);
        loop$key = key;
      } else if (new$1 instanceof Map) {
        let subtree = new$1.subtree;
        loop$diff = diff;
        loop$old = new Some(old$1);
        loop$new = new Some(subtree());
        loop$key = key;
      } else if (old$1 instanceof Text &&
      new$1 instanceof Text &&
      (old$1.content === new$1.content)) {
        let old$2 = old$1.content;
        let new$2 = new$1.content;
        return diff;
      } else if (old$1 instanceof Text && new$1 instanceof Text) {
        return diff.withFields({
          created: $dict.insert(diff.created, key, new$1)
        });
      } else if (old$1 instanceof Element && new$1 instanceof Text) {
        return diff.withFields({
          created: $dict.insert(diff.created, key, new$1)
        });
      } else if (old$1 instanceof Text && new$1 instanceof Element) {
        return diff.withFields({
          created: $dict.insert(diff.created, key, new$1),
          handlers: fold_event_handlers(diff.handlers, new$1, key)
        });
      } else if (old$1 instanceof Element &&
      new$1 instanceof Element &&
      ((old$1.namespace === new$1.namespace) && (old$1.tag === new$1.tag))) {
        let old_ns = old$1.namespace;
        let old_tag = old$1.tag;
        let old_attrs = old$1.attrs;
        let old_children = old$1.children;
        let new_ns = new$1.namespace;
        let new_tag = new$1.tag;
        let new_attrs = new$1.attrs;
        let new_children = new$1.children;
        let attribute_diff = attributes(old_attrs, new_attrs);
        let handlers = $dict.fold(
          attribute_diff.handlers,
          diff.handlers,
          (handlers, name, handler) => {
            let name$1 = $string.drop_left(name, 2);
            return $dict.insert(handlers, (key + "-") + name$1, handler);
          },
        );
        let diff$1 = diff.withFields({
          updated: (() => {
            let $ = is_empty_attribute_diff(attribute_diff);
            if ($) {
              return diff.updated;
            } else {
              return $dict.insert(diff.updated, key, attribute_diff);
            }
          })(),
          handlers: handlers
        });
        return do_element_list(diff$1, old_children, new_children, key);
      } else if (old$1 instanceof Element && new$1 instanceof Element) {
        return diff.withFields({
          created: $dict.insert(diff.created, key, new$1),
          handlers: fold_event_handlers(diff.handlers, new$1, key)
        });
      } else if (old$1 instanceof Fragment && new$1 instanceof Fragment) {
        let old_elements = old$1.elements;
        let new_elements = new$1.elements;
        return do_element_list(diff, old_elements, new_elements, key);
      } else if (new$1 instanceof Fragment) {
        return diff.withFields({
          created: $dict.insert(diff.created, key, new$1),
          handlers: fold_event_handlers(diff.handlers, new$1, key)
        });
      } else {
        return diff.withFields({
          created: $dict.insert(diff.created, key, new$1),
          handlers: fold_event_handlers(diff.handlers, new$1, key)
        });
      }
    }
  }
}

export function elements(old, new$) {
  return do_elements(
    new ElementDiff($dict.new$(), $set.new$(), $dict.new$(), $dict.new$()),
    new Some(old),
    new Some(new$),
    "0",
  );
}

function fold_element_list_event_handlers(handlers, elements, key) {
  return $list.index_fold(
    elements,
    handlers,
    (handlers, element, index) => {
      let key$1 = (key + "-") + $int.to_string(index);
      return fold_event_handlers(handlers, element, key$1);
    },
  );
}

function fold_event_handlers(loop$handlers, loop$element, loop$key) {
  while (true) {
    let handlers = loop$handlers;
    let element = loop$element;
    let key = loop$key;
    if (element instanceof Text) {
      return handlers;
    } else if (element instanceof Map) {
      let subtree = element.subtree;
      loop$handlers = handlers;
      loop$element = subtree();
      loop$key = key;
    } else if (element instanceof Element) {
      let attrs = element.attrs;
      let children = element.children;
      let handlers$1 = $list.fold(
        attrs,
        handlers,
        (handlers, attr) => {
          let $ = event_handler(attr);
          if ($.isOk()) {
            let name = $[0][0];
            let handler = $[0][1];
            return $dict.insert(handlers, (key + "-") + name, handler);
          } else {
            return handlers;
          }
        },
      );
      return fold_element_list_event_handlers(handlers$1, children, key);
    } else {
      let elements$1 = element.elements;
      return fold_element_list_event_handlers(handlers, elements$1, key);
    }
  }
}
