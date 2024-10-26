//// This is the module used to generate the json interface
//// used by the tests!
////

/// Documentation!
pub type Wibble(a) {
  /// Documentation!
  Wibble(label: a)
  /// Documentation!
  Wobble(a)
  Wabble
}

/// Documentation!
@deprecated("this is deprecated!")
pub type Wobble =
  Wibble(Int)

/// Documentation!
pub const wibble = Wibble(1)

pub const wobble = #(1, 1)

pub const wabble = main

/// Documentation!
pub fn main(wibble _wobble: String) -> Wibble(a) {
  Wabble
}
