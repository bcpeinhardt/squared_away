pub type Repeater(state)

/// Call a function every specified number of milliseconds, waiting the number
/// of milliseconds before the first call.
///
@external(erlang, "repeatedly_ffi", "call")
@external(javascript, "./repeatedly_ffi.mjs", "call")
pub fn call(
  delay_ms: Int,
  state: state,
  function: fn(state, Int) -> a,
) -> Repeater(state)

/// Stop the repeater, preventing it from triggering again.
///
/// On Erlang if the repeater message queue is not empty then this message will
/// handled after all other messages.
///
/// On JavaScript there is no message queue so it will stop immediately, though
/// not interrupt the function callback if currently being executed.
///
@external(erlang, "repeatedly_ffi", "stop")
@external(javascript, "./repeatedly_ffi.mjs", "stop")
pub fn stop(repeater: Repeater(state)) -> Nil

/// Replace the function being called by a repeater.
///
/// On Erlang if the repeater message queue is not empty then this message will
/// handled after all other messages.
///
/// On JavaScript there is no message queue so it will stop immediately, though
/// not interrupt the function callback if currently being executed.
///
@external(erlang, "repeatedly_ffi", "replace")
@external(javascript, "./repeatedly_ffi.mjs", "replace")
pub fn set_function(
  repeater: Repeater(state),
  function: fn(state, Int) -> a,
) -> Nil

/// Set the repeater state.
///
/// On Erlang if the repeater message queue is not empty then this message will
/// handled after all other messages.
///
/// On JavaScript there is no message queue so it will stop immediately, though
/// not interrupt the function callback if currently being executed.
///
pub fn set_state(repeater: Repeater(state), state: state) -> Nil {
  update_state(repeater, fn(_state) { state })
}

/// Update the repeater state.
///
/// On Erlang if the repeater message queue is not empty then this message will
/// handled after all other messages.
///
/// On JavaScript there is no message queue so it will stop immediately, though
/// not interrupt the function callback if currently being executed.
///
@external(erlang, "repeatedly_ffi", "update_state")
@external(javascript, "./repeatedly_ffi.mjs", "update_state")
pub fn update_state(
  repeater: Repeater(state),
  function: fn(state) -> state,
) -> Nil
