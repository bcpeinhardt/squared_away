import gleam/bool
import gleam/float
import gleam/int
import gleam/string

pub type Value {
  Empty
  Text(inner: String)
  Integer(n: Int)
  FloatingPointNumber(f: Float)
  Usd(cents: Int)
  Boolean(b: Bool)
  TestFail
  TestPass
}

pub fn value_to_string(fv: Value) -> String {
  case fv {
    Empty -> ""
    Text(t) -> t
    Integer(n) -> int.to_string(n)
    Boolean(b) -> bool.to_string(b) |> string.uppercase
    FloatingPointNumber(f) -> float.to_string(f)
    TestFail -> "Test Failure"
    TestPass -> "Test Passing"
    Usd(cents) -> {
      let dollars = int.to_string(cents / 100)
      let cents = int.to_string(cents % 100)
      let cents = case string.length(cents) {
        1 -> cents <> "0"
        2 -> cents 
        _ -> panic as "This shit shouldn't happen"
      }

      "$" <> dollars <> "." <> cents
    }
  }
}
