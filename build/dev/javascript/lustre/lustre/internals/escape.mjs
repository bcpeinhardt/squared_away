import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { toList, prepend as listPrepend } from "../../gleam.mjs";
import { first, drop_first, slice } from "../../lustre-escape.ffi.mjs";

function do_escape(
  loop$string,
  loop$skip,
  loop$original,
  loop$acc,
  loop$len,
  loop$found_normal
) {
  while (true) {
    let string = loop$string;
    let skip = loop$skip;
    let original = loop$original;
    let acc = loop$acc;
    let len = loop$len;
    let found_normal = loop$found_normal;
    let $ = first(string);
    if (!found_normal && $ === "<") {
      let rest = drop_first(string);
      let acc$1 = listPrepend("&lt;", acc);
      loop$string = rest;
      loop$skip = skip + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (!found_normal && $ === ">") {
      let rest = drop_first(string);
      let acc$1 = listPrepend("&gt;", acc);
      loop$string = rest;
      loop$skip = skip + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (!found_normal && $ === "&") {
      let rest = drop_first(string);
      let acc$1 = listPrepend("&amp;", acc);
      loop$string = rest;
      loop$skip = skip + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (!found_normal && $ === "\"") {
      let rest = drop_first(string);
      let acc$1 = listPrepend("&quot;", acc);
      loop$string = rest;
      loop$skip = skip + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (!found_normal && $ === "'") {
      let rest = drop_first(string);
      let acc$1 = listPrepend("&#39;", acc);
      loop$string = rest;
      loop$skip = skip + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (!found_normal && $ === "") {
      return acc;
    } else if (!found_normal) {
      let rest = drop_first(string);
      loop$string = rest;
      loop$skip = skip;
      loop$original = original;
      loop$acc = acc;
      loop$len = 1;
      loop$found_normal = true;
    } else if (found_normal && $ === "<") {
      let rest = drop_first(string);
      let slice$1 = slice(original, skip, len);
      let acc$1 = listPrepend("&lt;", listPrepend(slice$1, acc));
      loop$string = rest;
      loop$skip = (skip + len) + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (found_normal && $ === ">") {
      let rest = drop_first(string);
      let slice$1 = slice(original, skip, len);
      let acc$1 = listPrepend("&gt;", listPrepend(slice$1, acc));
      loop$string = rest;
      loop$skip = (skip + len) + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (found_normal && $ === "&") {
      let rest = drop_first(string);
      let slice$1 = slice(original, skip, len);
      let acc$1 = listPrepend("&amp;", listPrepend(slice$1, acc));
      loop$string = rest;
      loop$skip = (skip + len) + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (found_normal && $ === "\"") {
      let rest = drop_first(string);
      let slice$1 = slice(original, skip, len);
      let acc$1 = listPrepend("&quot;", listPrepend(slice$1, acc));
      loop$string = rest;
      loop$skip = (skip + len) + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (found_normal && $ === "'") {
      let rest = drop_first(string);
      let slice$1 = slice(original, skip, len);
      let acc$1 = listPrepend("&#39;", listPrepend(slice$1, acc));
      loop$string = rest;
      loop$skip = (skip + len) + 1;
      loop$original = original;
      loop$acc = acc$1;
      loop$len = 0;
      loop$found_normal = false;
    } else if (found_normal && $ === "") {
      if (skip === 0) {
        return toList([original]);
      } else {
        let slice$1 = slice(original, skip, len);
        return listPrepend(slice$1, acc);
      }
    } else {
      let rest = drop_first(string);
      loop$string = rest;
      loop$skip = skip;
      loop$original = original;
      loop$acc = acc;
      loop$len = len + 1;
      loop$found_normal = true;
    }
  }
}

export function escape(text) {
  let _pipe = do_escape(text, 0, text, toList([]), 0, false);
  let _pipe$1 = $list.reverse(_pipe);
  return $string.join(_pipe$1, "");
}
