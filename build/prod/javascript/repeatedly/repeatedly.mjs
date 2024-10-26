import { call, stop, replace as set_function, update_state } from "./repeatedly_ffi.mjs";

export { call, set_function, stop, update_state };

export function set_state(repeater, state) {
  return update_state(repeater, (_) => { return state; });
}
