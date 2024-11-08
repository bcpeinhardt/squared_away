import bigi
import gleam/bool
import gleam/float
import gleam/int
import gleam/order
import gleam/string

pub type Value {
  Empty
  Text(inner: String)
  Integer(n: Int)
  FloatingPointNumber(f: Float)
  Usd(cents: bigi.BigInt)
  Percent(numerator: bigi.BigInt, denominator: bigi.BigInt)
  Boolean(b: Bool)
  TestFail
  TestPass
}

fn normalize_percent(numerator: bigi.BigInt, denominator: bigi.BigInt) -> String {
  do_normalize_percent(bigi.to_string(numerator), denominator)
}

fn do_normalize_percent(n: String, d: bigi.BigInt) -> String {
  case bigi.from_int(100) |> bigi.compare(d) {
    order.Eq -> n
    order.Gt -> panic as "shouldn't happen dawg check the typed_expr module"
    order.Lt -> {
      let next_n = bigi.modulo(d, bigi.from_int(10))
      do_normalize_percent(
        n <> bigi.to_string(next_n),
        bigi.divide(d, bigi.from_int(10)),
      )
    }
  }
}

pub fn value_to_string(fv: Value) -> String {
  case fv {
    Empty -> ""
    Text(t) -> t
    Integer(n) -> int.to_string(n)
    Boolean(b) -> bool.to_string(b) |> string.uppercase
    FloatingPointNumber(f) -> float.to_string(f)
    Percent(n, d) -> normalize_percent(n, d) <> "%"
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
