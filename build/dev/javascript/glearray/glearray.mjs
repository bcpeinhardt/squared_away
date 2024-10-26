import * as $iterator from "../gleam_stdlib/gleam/iterator.mjs";
import { toList as to_list, Ok, Error } from "./gleam.mjs";
import {
  newArray as new$,
  fromList as from_list,
  arrayLength as length,
  get as do_get,
  set as do_set,
  push as copy_push,
  insert as do_insert,
} from "./glearray_ffi.mjs";

export { copy_push, from_list, length, new$, to_list };

function is_valid_index(array, index) {
  return (index >= 0) && (index < length(array));
}

export function get(array, index) {
  let $ = is_valid_index(array, index);
  if ($) {
    return new Ok(do_get(array, index));
  } else {
    return new Error(undefined);
  }
}

export function copy_set(array, index, value) {
  let $ = is_valid_index(array, index);
  if ($) {
    return new Ok(do_set(array, index, value));
  } else {
    return new Error(undefined);
  }
}

export function copy_insert(array, index, value) {
  let $ = (index >= 0) && (index <= length(array));
  if ($) {
    return new Ok(do_insert(array, index, value));
  } else {
    return new Error(undefined);
  }
}

export function iterate(array) {
  return $iterator.unfold(
    0,
    (index) => {
      let $ = get(array, index);
      if ($.isOk()) {
        let element = $[0];
        return new $iterator.Next(element, index + 1);
      } else {
        return new $iterator.Done();
      }
    },
  );
}
