import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { toList, prepend as listPrepend } from "../../gleam.mjs";

export function max_string_length(strings) {
  return $list.fold(
    strings,
    0,
    (max, f) => {
      let _pipe = f;
      let _pipe$1 = $string.length(_pipe);
      return $int.max(_pipe$1, max);
    },
  );
}

function do_wordwrap(loop$tokens, loop$max_width, loop$line, loop$lines) {
  while (true) {
    let tokens = loop$tokens;
    let max_width = loop$max_width;
    let line = loop$line;
    let lines = loop$lines;
    if (tokens.atLeastLength(1)) {
      let token = tokens.head;
      let tokens$1 = tokens.tail;
      let token_length = $string.length(token);
      let line_length = $string.length(line);
      let $ = ((line_length + 1) + token_length) <= max_width;
      if (line === "") {
        loop$tokens = tokens$1;
        loop$max_width = max_width;
        loop$line = token;
        loop$lines = lines;
      } else if ($) {
        loop$tokens = tokens$1;
        loop$max_width = max_width;
        loop$line = (line + " ") + token;
        loop$lines = lines;
      } else {
        loop$tokens = tokens$1;
        loop$max_width = max_width;
        loop$line = token;
        loop$lines = listPrepend(line, lines);
      }
    } else if (tokens.hasLength(0) && (line === "")) {
      return $list.reverse(lines);
    } else {
      return $list.reverse(listPrepend(line, lines));
    }
  }
}

function space_split_lines(s) {
  let chunks = (() => {
    let _pipe = s;
    let _pipe$1 = $string.trim(_pipe);
    let _pipe$2 = $string.to_graphemes(_pipe$1);
    return $list.chunk(_pipe$2, (s) => { return s === "\n"; });
  })();
  let lines = $list.fold(
    chunks,
    [toList([]), false],
    (acc, chunk) => {
      let $ = acc[0];
      if (chunk.atLeastLength(2) &&
      chunk.head === "\n" &&
      chunk.tail.head === "\n" &&
      $.atLeastLength(1)) {
        let rest = chunk.tail.tail;
        let s$1 = $.head;
        let accs = $.tail;
        return [listPrepend(s$1 + $string.concat(rest), accs), true];
      } else if (chunk.hasLength(1) && chunk.head === "\n" && $.atLeastLength(1)) {
        let s$1 = $.head;
        let accs = $.tail;
        return [listPrepend(s$1 + " ", accs), false];
      } else if ($.atLeastLength(1) && (!acc[1])) {
        let s$1 = $.head;
        let accs = $.tail;
        return [listPrepend(s$1 + $string.concat(chunk), accs), false];
      } else {
        return [listPrepend($string.concat(chunk), acc[0]), false];
      }
    },
  );
  return $list.reverse(lines[0]);
}

export function wordwrap(s, max_width) {
  return $bool.guard(
    s === "",
    toList([]),
    () => {
      return $list.flat_map(
        space_split_lines(s),
        (line) => {
          let _pipe = line;
          let _pipe$1 = $string.split(_pipe, " ");
          return do_wordwrap(_pipe$1, max_width, "", toList([]));
        },
      );
    },
  );
}
