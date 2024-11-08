import gleam/bool
import gleam/float
import gleam/int
import gleam/string
import squared_away/squared_away_lang/util/rational

pub type Value {
  Empty
  Text(inner: String)
  Integer(n: Int)
  FloatingPointNumber(f: Float)
  Usd(cents: rational.Rat)
  Percent(percent: rational.Rat)
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
    Percent(p) ->
      rational.to_string(rational.multiply(p, rational.from_int(100)), 10)
      <> "%"
    TestFail -> "Test Failure"
    TestPass -> "Test Passing"
    Usd(cents) -> {
      "$" <> rational.to_string(cents, 2)
    }
  }
}
