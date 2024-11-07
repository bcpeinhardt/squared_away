import bigi
import gleam/bool
import gleam/float
import gleam/int
import gleam/string

pub type Value {
  Empty
  Text(inner: String)
  Integer(n: Int)
  FloatingPointNumber(f: Float)
  Usd(cents: bigi.BigInt)
  Percent(percent: Int)
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
    Percent(p) -> int.to_string(p) <> "%"
    TestFail -> "Test Failure"
    TestPass -> "Test Passing"
    Usd(cents) -> {
      let dollars = bigi.divide(cents, bigi.from_int(100)) |> bigi.to_string
      let cents = bigi.modulo(cents, bigi.from_int(100)) |> bigi.to_string
      let cents = case string.length(cents) {
        1 -> cents <> "0"
        2 -> cents
        _ -> panic as "This shit shouldn't happen"
      }

      "$" <> dollars <> "." <> cents
    }
  }
}
