import gleam/bool
import gleam/float
import gleam/int

pub type Value {
  Empty
  Text(inner: String)
  Integer(n: Int)
  FloatingPointNumber(f: Float)
  Boolean(b: Bool)
}

pub fn value_to_string(fv: Value) -> String {
  case fv {
    Empty -> ""
    Text(t) -> t
    Integer(n) -> int.to_string(n)
    Boolean(b) -> bool.to_string(b)
    FloatingPointNumber(f) -> float.to_string(f)
  }
}
