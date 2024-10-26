import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { is_windows } from "./filepath_ffi.mjs";
import { Ok, Error, toList, prepend as listPrepend, isEqual } from "./gleam.mjs";

function relative(loop$path) {
  while (true) {
    let path = loop$path;
    if (path.startsWith("/")) {
      let path$1 = path.slice(1);
      loop$path = path$1;
    } else {
      return path;
    }
  }
}

function remove_trailing_slash(path) {
  let $ = $string.ends_with(path, "/");
  if ($) {
    return $string.drop_right(path, 1);
  } else {
    return path;
  }
}

export function join(left, right) {
  let _pipe = (() => {
    if (right === "/") {
      return left;
    } else if (left === "") {
      return relative(right);
    } else if (left === "/") {
      return right;
    } else {
      let _pipe = remove_trailing_slash(left);
      let _pipe$1 = $string.append(_pipe, "/");
      return $string.append(_pipe$1, relative(right));
    }
  })();
  return remove_trailing_slash(_pipe);
}

export function split_unix(path) {
  let _pipe = (() => {
    let $ = $string.split(path, "/");
    if ($.hasLength(1) && $.head === "") {
      return toList([]);
    } else if ($.atLeastLength(1) && $.head === "") {
      let rest = $.tail;
      return listPrepend("/", rest);
    } else {
      let rest = $;
      return rest;
    }
  })();
  return $list.filter(_pipe, (x) => { return x !== ""; });
}

function pop_windows_drive_specifier(path) {
  let start = $string.slice(path, 0, 3);
  let codepoints = $string.to_utf_codepoints(start);
  let $ = $list.map(codepoints, $string.utf_codepoint_to_int);
  if ($.hasLength(3) &&
  (((($.tail.tail.head === 47) || ($.tail.tail.head === 92)) && ($.tail.head === 58)) && ((($.head >= 65) && ($.head <= 90)) || (($.head >= 97) && ($.head <= 122))))) {
    let drive = $.head;
    let colon = $.tail.head;
    let slash = $.tail.tail.head;
    let drive_letter = $string.slice(path, 0, 1);
    let drive$1 = $string.lowercase(drive_letter) + ":/";
    let path$1 = $string.drop_left(path, 3);
    return [new Some(drive$1), path$1];
  } else {
    return [new None(), path];
  }
}

export function split_windows(path) {
  let $ = pop_windows_drive_specifier(path);
  let drive = $[0];
  let path$1 = $[1];
  let segments = (() => {
    let _pipe = $string.split(path$1, "/");
    return $list.flat_map(
      _pipe,
      (_capture) => { return $string.split(_capture, "\\"); },
    );
  })();
  let segments$1 = (() => {
    if (drive instanceof Some) {
      let drive$1 = drive[0];
      return listPrepend(drive$1, segments);
    } else {
      return segments;
    }
  })();
  if (segments$1.hasLength(1) && segments$1.head === "") {
    return toList([]);
  } else if (segments$1.atLeastLength(1) && segments$1.head === "") {
    let rest = segments$1.tail;
    return listPrepend("/", rest);
  } else {
    let rest = segments$1;
    return rest;
  }
}

export function split(path) {
  let $ = is_windows();
  if ($) {
    return split_windows(path);
  } else {
    return split_unix(path);
  }
}

export function base_name(path) {
  return $bool.guard(
    path === "/",
    "",
    () => {
      let _pipe = path;
      let _pipe$1 = split(_pipe);
      let _pipe$2 = $list.last(_pipe$1);
      return $result.unwrap(_pipe$2, "");
    },
  );
}

export function extension(path) {
  let file = base_name(path);
  let $ = $string.split(file, ".");
  if ($.hasLength(2) && $.head === "") {
    return new Error(undefined);
  } else if ($.hasLength(2)) {
    let extension$1 = $.tail.head;
    return new Ok(extension$1);
  } else if ($.atLeastLength(1)) {
    let rest = $.tail;
    return $list.last(rest);
  } else {
    return new Error(undefined);
  }
}

export function strip_extension(path) {
  let $ = extension(path);
  if ($.isOk()) {
    let extension$1 = $[0];
    return $string.drop_right(path, $string.length(extension$1) + 1);
  } else {
    return path;
  }
}

function get_directory_name(loop$path, loop$acc, loop$segment) {
  while (true) {
    let path = loop$path;
    let acc = loop$acc;
    let segment = loop$segment;
    if (path.atLeastLength(1) && path.head === "/") {
      let rest = path.tail;
      loop$path = rest;
      loop$acc = acc + segment;
      loop$segment = "/";
    } else if (path.atLeastLength(1)) {
      let first = path.head;
      let rest = path.tail;
      loop$path = rest;
      loop$acc = acc;
      loop$segment = segment + first;
    } else {
      return acc;
    }
  }
}

export function directory_name(path) {
  let path$1 = remove_trailing_slash(path);
  if (path$1.startsWith("/")) {
    let rest = path$1.slice(1);
    return get_directory_name($string.to_graphemes(rest), "/", "");
  } else {
    return get_directory_name($string.to_graphemes(path$1), "", "");
  }
}

export function is_absolute(path) {
  return $string.starts_with(path, "/");
}

function expand_segments(loop$path, loop$base) {
  while (true) {
    let path = loop$path;
    let base = loop$base;
    if (base.hasLength(1) &&
    base.head === "" &&
    path.atLeastLength(1) &&
    path.head === "..") {
      return new Error(undefined);
    } else if (base.hasLength(0) && path.atLeastLength(1) && path.head === "..") {
      return new Error(undefined);
    } else if (base.atLeastLength(1) &&
    path.atLeastLength(1) &&
    path.head === "..") {
      let base$1 = base.tail;
      let path$1 = path.tail;
      loop$path = path$1;
      loop$base = base$1;
    } else if (path.atLeastLength(1) && path.head === ".") {
      let path$1 = path.tail;
      loop$path = path$1;
      loop$base = base;
    } else if (path.atLeastLength(1)) {
      let s = path.head;
      let path$1 = path.tail;
      loop$path = path$1;
      loop$base = listPrepend(s, base);
    } else {
      return new Ok($string.join($list.reverse(base), "/"));
    }
  }
}

function root_slash_to_empty(segments) {
  if (segments.atLeastLength(1) && segments.head === "/") {
    let rest = segments.tail;
    return listPrepend("", rest);
  } else {
    return segments;
  }
}

export function expand(path) {
  let is_absolute$1 = is_absolute(path);
  let result = (() => {
    let _pipe = path;
    let _pipe$1 = split(_pipe);
    let _pipe$2 = root_slash_to_empty(_pipe$1);
    return expand_segments(_pipe$2, toList([]));
  })();
  let $ = is_absolute$1 && (isEqual(result, new Ok("")));
  if ($) {
    return new Ok("/");
  } else {
    return result;
  }
}
