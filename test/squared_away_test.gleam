import gleeunit
import gleeunit/should
import squared_away/squared_away_lang/util/rational

pub fn main() {
  gleeunit.main()
}

// Rational number tests
pub fn rat_test() {
  let two = rational.from_int(2)
  let ten = rational.from_int(10)
  rational.add(two, ten) |> should.equal(rational.from_int(12))

  let #(one_three, _) = rational.from_string("1.3") |> should.be_ok
  let #(one_seven, _) = rational.from_string("1.7") |> should.be_ok
  rational.add(one_three, one_seven) |> should.equal(rational.from_int(3))

  rational.from_string("*6") |> should.be_error

  let two_thirds = rational.from_ints(6, 9)
  let one_third = rational.from_ints(3, 9)

  rational.add(one_third, two_thirds) |> should.equal(rational.from_int(1))

  rational.subtract(one_third, two_thirds)
  |> should.equal(rational.from_ints(-1, 3))
  rational.subtract(one_third, two_thirds)
  |> should.equal(rational.from_ints(1, -3))
  rational.from_ints(-1, 3) |> should.equal(rational.from_ints(1, -3))

  rational.multiply(two_thirds, one_third)
  |> should.equal(rational.from_ints(2, 9))

  rational.divide(one_third, two_thirds)
  |> should.equal(rational.from_ints(1, 2))

  rational.to_string(two_thirds, 10, False) |> should.equal("0.6666666666")
}
