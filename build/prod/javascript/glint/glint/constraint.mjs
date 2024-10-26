import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $snag from "../../snag/snag.mjs";
import { Ok } from "../gleam.mjs";

export function one_of(allowed) {
  let allowed_set = $set.from_list(allowed);
  return (val) => {
    let $ = $set.contains(allowed_set, val);
    if ($) {
      return new Ok(val);
    } else {
      return $snag.error(
        ((("invalid value '" + $string.inspect(val)) + "', must be one of: [") + (() => {
          let _pipe = allowed;
          let _pipe$1 = $list.map(
            _pipe,
            (a) => { return ("'" + $string.inspect(a)) + "'"; },
          );
          return $string.join(_pipe$1, ", ");
        })()) + "]",
      );
    }
  };
}

export function none_of(disallowed) {
  let disallowed_set = $set.from_list(disallowed);
  return (val) => {
    let $ = $set.contains(disallowed_set, val);
    if (!$) {
      return new Ok(val);
    } else {
      return $snag.error(
        (("invalid value '" + $string.inspect(val)) + "', must not be one of: [") + ((() => {
          let _pipe = disallowed;
          let _pipe$1 = $list.map(
            _pipe,
            (a) => { return ("'" + $string.inspect(a)) + "'"; },
          );
          return $string.join(_pipe$1, ", ");
        })() + "]"),
      );
    }
  };
}

export function each(constraint) {
  return (_capture) => { return $list.try_map(_capture, constraint); };
}
