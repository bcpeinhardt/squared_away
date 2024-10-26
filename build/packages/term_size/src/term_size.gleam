//// All functions return `Error(Nil)` if the terminal's size could not be
//// determined for any reason.

/// Get the current terminal's size. The return tuple is of the form
/// `#(rows, columns)`.
///
///
/// ```
/// import term_size
///
/// pub fn main() {
///   case term_size.get() {
///     Ok(#(rows, columns)) -> // ...
///     Error(Nil) -> // ...
///   }
/// }
/// ```
///
@external(erlang, "term_size_ffi", "terminal_size")
@external(javascript, "./term_size_ffi.mjs", "terminal_size")
pub fn get() -> Result(#(Int, Int), Nil)

/// Get the number of rows (lines) visible in the terminal.
///
pub fn rows() -> Result(Int, Nil) {
  case get() {
    Ok(#(rows, _)) -> Ok(rows)
    Error(Nil) -> Error(Nil)
  }
}

/// Get the character width of the terminal.
///
pub fn columns() -> Result(Int, Nil) {
  case get() {
    Ok(#(_, columns)) -> Ok(columns)
    Error(Nil) -> Error(Nil)
  }
}
