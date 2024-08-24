import form/lang
import form/lang/interpreter
import form/lang/parser
import form/lang/scanner
import gleam/dict
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn scanner_test() {
  let test_cases = [
    #("=-+*/", [scanner.Minus, scanner.Plus, scanner.Star, scanner.Div]),
    #("=   - >= + * / ** = ! && || != ( <= ) == <> TRUE FALSE", [
      scanner.Minus,
      scanner.GreaterEqual,
      scanner.Plus,
      scanner.Star,
      scanner.Div,
      scanner.StarStar,
      scanner.Equal,
      scanner.Bang,
      scanner.And,
      scanner.Or,
      scanner.BangEqual,
      scanner.LParen,
      scanner.LessEqual,
      scanner.RParen,
      scanner.EqualEqual,
      scanner.Less,
      scanner.Greater,
      scanner.TrueToken,
      scanner.FalseToken,
    ]),
    #("=   - AS45 + 786", [
      scanner.Minus,
      scanner.CellReference("AS45"),
      scanner.Plus,
      scanner.IntegerLiteral(786),
    ]),
    #("+-*/=", [scanner.StringLiteral("+-*/=")]),
  ]

  use tc <- list.each(test_cases)
  tc.0 |> scanner.scan |> should.be_ok |> should.equal(tc.1)
}

pub fn parser_test() {
  let test_cases = [
    #(
      [
        scanner.IntegerLiteral(7),
        scanner.Plus,
        scanner.IntegerLiteral(8),
        scanner.Minus,
        scanner.IntegerLiteral(9),
      ],
      parser.BinaryOp(
        parser.BinaryOp(
          parser.IntegerLiteral(7),
          parser.Add,
          parser.IntegerLiteral(8),
        ),
        parser.Subtract,
        parser.IntegerLiteral(9),
      ),
    ),
    #(
      [
        scanner.IntegerLiteral(1),
        scanner.Plus,
        scanner.IntegerLiteral(2),
        scanner.Plus,
        scanner.IntegerLiteral(3),
        scanner.Plus,
        scanner.IntegerLiteral(4),
        scanner.Plus,
        scanner.IntegerLiteral(5),
        scanner.Plus,
        scanner.IntegerLiteral(6),
      ],
      parser.BinaryOp(
        parser.BinaryOp(
          parser.BinaryOp(
            parser.BinaryOp(
              parser.BinaryOp(
                parser.IntegerLiteral(1),
                parser.Add,
                parser.IntegerLiteral(2),
              ),
              parser.Add,
              parser.IntegerLiteral(3),
            ),
            parser.Add,
            parser.IntegerLiteral(4),
          ),
          parser.Add,
          parser.IntegerLiteral(5),
        ),
        parser.Add,
        parser.IntegerLiteral(6),
      ),
    ),
  ]

  use tc <- list.each(test_cases)
  tc.0 |> parser.parse |> should.be_ok |> should.equal(tc.1)
}

pub fn integration_lang_test() {
  let test_cases = [
    #("=27+4-10", interpreter.Integer(21)),
    #("=TRUE && FALSE", interpreter.Boolean(False)),
    // TODO: Fix this parser issue
  // #("=2+(5*8)", interpreter.Integer(42))
  ]

  use tc <- list.each(test_cases)
  let assert Ok(tokens) = scanner.scan(tc.0)
  let assert Ok(expr) = parser.parse(tokens)
  interpreter.interpret(dict.new(), expr) |> should.be_ok |> should.equal(tc.1)
}
