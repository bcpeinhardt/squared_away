import gleam/dict
import gleam/list
import gleeunit
import gleeunit/should
import squared_away/lang/interpreter
import squared_away/lang/interpreter/value
import squared_away/lang/parser
import squared_away/lang/parser/expr
import squared_away/lang/scanner
import squared_away/lang/scanner/token
import squared_away/lang/typechecker

pub fn main() {
  gleeunit.main()
}

pub fn scanner_test() {
  let test_cases = [
    #("=-+*/", [token.Minus, token.Plus, token.Star, token.Div]),
    #("=   - >= + * / ** = ! && || != ( <= ) == <> TRUE FALSE", [
      token.Minus,
      token.GreaterEqual,
      token.Plus,
      token.Star,
      token.Div,
      token.StarStar,
      token.Equal,
      token.Bang,
      token.And,
      token.Or,
      token.BangEqual,
      token.LParen,
      token.LessEqual,
      token.RParen,
      token.EqualEqual,
      token.Less,
      token.Greater,
      token.TrueToken,
      token.FalseToken,
    ]),
    #("=   - AS45 + 786", [
      token.Minus,
      token.CellReference("AS45"),
      token.Plus,
      token.IntegerLiteral(786),
    ]),
    #("+-*/=", [token.Label("+-*/=")]),
  ]

  use tc <- list.each(test_cases)
  tc.0 |> scanner.scan |> should.be_ok |> should.equal(tc.1)
}

pub fn parser_test() {
  let test_cases = [
    #(
      [
        token.IntegerLiteral(7),
        token.Plus,
        token.IntegerLiteral(8),
        token.Minus,
        token.IntegerLiteral(9),
      ],
      expr.BinaryOp(
        expr.IntegerLiteral(7),
        expr.Add,
        expr.BinaryOp(
          expr.IntegerLiteral(8),
          expr.Subtract,
          expr.IntegerLiteral(9),
        ),
      ),
    ),
    #(
      [
        token.IntegerLiteral(1),
        token.Plus,
        token.IntegerLiteral(2),
        token.Plus,
        token.IntegerLiteral(3),
        token.Plus,
        token.IntegerLiteral(4),
        token.Plus,
        token.IntegerLiteral(5),
        token.Plus,
        token.IntegerLiteral(6),
      ],
      expr.BinaryOp(
        expr.IntegerLiteral(1),
        expr.Add,
        expr.BinaryOp(
          expr.IntegerLiteral(2),
          expr.Add,
          expr.BinaryOp(
            expr.IntegerLiteral(3),
            expr.Add,
            expr.BinaryOp(
              expr.IntegerLiteral(4),
              expr.Add,
              expr.BinaryOp(
                expr.IntegerLiteral(5),
                expr.Add,
                expr.IntegerLiteral(6),
              ),
            ),
          ),
        ),
      ),
    ),
  ]

  use tc <- list.each(test_cases)
  tc.0 |> parser.parse |> should.be_ok |> should.equal(tc.1)
}

pub fn integration_lang_test() {
  let test_cases = [
    #("=27+4-10", value.Integer(21)),
    #("=TRUE && FALSE", value.Boolean(False)),
    #("=2+(5*8)", value.Integer(42)),
  ]

  use tc <- list.each(test_cases)
  let assert Ok(tokens) = scanner.scan(tc.0)
  let assert Ok(expr) = parser.parse(tokens)
  let assert Ok(typed_expr) = typechecker.typecheck(dict.new(), expr)
  interpreter.interpret(dict.new(), typed_expr)
  |> should.be_ok
  |> should.equal(tc.1)
}
