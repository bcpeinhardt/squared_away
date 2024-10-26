import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $term_size from "../../term_size/term_size.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  divideInt,
} from "../gleam.mjs";

class Left extends $CustomType {}

class Right extends $CustomType {}

export function term_width() {
  let _pipe = $term_size.columns();
  return $result.unwrap(_pipe, 80);
}

function do_shorten(
  loop$left,
  loop$right,
  loop$shortened,
  loop$from,
  loop$current_length,
  loop$max_length
) {
  while (true) {
    let left = loop$left;
    let right = loop$right;
    let shortened = loop$shortened;
    let from = loop$from;
    let current_length = loop$current_length;
    let max_length = loop$max_length;
    let $ = current_length <= max_length;
    if ($) {
      if (shortened) {
        return new Ok([left, right]);
      } else {
        return new Error(undefined);
      }
    } else if (left.hasLength(0) && right.hasLength(1)) {
      if (shortened) {
        return new Ok([left, right]);
      } else {
        return new Error(undefined);
      }
    } else if (left.hasLength(0) && right.hasLength(0)) {
      if (shortened) {
        return new Ok([left, right]);
      } else {
        return new Error(undefined);
      }
    } else if (right.hasLength(1) && from instanceof Right) {
      let left$1 = left;
      let right$1 = right;
      loop$left = left$1;
      loop$right = right$1;
      loop$shortened = shortened;
      loop$from = new Left();
      loop$current_length = current_length;
      loop$max_length = max_length;
    } else if (left.atLeastLength(1) && from instanceof Left) {
      let dropped = left.head;
      let left$1 = left.tail;
      let right$1 = right;
      let new_length = current_length - $string.length(dropped);
      loop$left = left$1;
      loop$right = right$1;
      loop$shortened = true;
      loop$from = new Right();
      loop$current_length = new_length;
      loop$max_length = max_length;
    } else if (right.atLeastLength(1) && from instanceof Right) {
      let left$1 = left;
      let dropped = right.head;
      let right$1 = right.tail;
      let new_length = current_length - $string.length(dropped);
      loop$left = left$1;
      loop$right = right$1;
      loop$shortened = true;
      loop$from = new Left();
      loop$current_length = new_length;
      loop$max_length = max_length;
    } else if (left.hasLength(0) && from instanceof Left) {
      let right$1 = right;
      loop$left = toList([]);
      loop$right = right$1;
      loop$shortened = shortened;
      loop$from = new Right();
      loop$current_length = current_length;
      loop$max_length = max_length;
    } else {
      let left$1 = left;
      loop$left = left$1;
      loop$right = toList([]);
      loop$shortened = shortened;
      loop$from = new Left();
      loop$current_length = current_length;
      loop$max_length = max_length;
    }
  }
}

function shorten(strings, max_length) {
  let initial_length = $list.fold(
    strings,
    0,
    (acc, string) => { return acc + $string.length(string); },
  );
  let middle = divideInt($list.length(strings), 2);
  let $ = $list.split(strings, middle);
  let left = $[0];
  let right = $[1];
  let left$1 = $list.reverse(left);
  let $1 = do_shorten(
    left$1,
    right,
    false,
    new Right(),
    initial_length,
    max_length,
  );
  if ($1.isOk()) {
    let new_left = $1[0][0];
    let new_right = $1[0][1];
    return new Ok([$list.reverse(new_left), new_right]);
  } else {
    return new Error(undefined);
  }
}

export function shorten_url(url, max_length) {
  let chunks = (() => {
    if (url.startsWith("https://")) {
      let rest = url.slice(8);
      return listPrepend("https:/", $string.split(rest, "/"));
    } else {
      return $string.split(url, "/");
    }
  })();
  let max_length$1 = max_length - $list.length(chunks);
  let $ = shorten(chunks, max_length$1);
  if (!$.isOk()) {
    return url;
  } else {
    let left = $[0][0];
    let right = $[0][1];
    return ($string.join(left, "/") + "/.../") + $string.join(right, "/");
  }
}
