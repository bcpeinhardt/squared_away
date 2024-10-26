import { Ok, Error } from "./gleam.mjs";
import { terminal_size as get } from "./term_size_ffi.mjs";

export { get };

export function rows() {
  let $ = get();
  if ($.isOk()) {
    let rows$1 = $[0][0];
    return new Ok(rows$1);
  } else {
    return new Error(undefined);
  }
}

export function columns() {
  let $ = get();
  if ($.isOk()) {
    let columns$1 = $[0][1];
    return new Ok(columns$1);
  } else {
    return new Error(undefined);
  }
}
