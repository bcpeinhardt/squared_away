import gleam/bool
import gleam/dict
import gleam/list
import gleam/result
import gleam/string

type ScanState {
  ScanState(row: Int, col: Int, keywords: dict.Dict(String, TokenType))
}

pub type TokenType {
  // Prefix 
  Equal
  Bang
  LParen
  Usd

  // Infix
  Plus
  Minus
  Star
  StarStar
  Slash
  BangEqual
  EqualEqual
  Less
  LessEqual
  Greater
  GreaterEqual
  And
  Or
  MustBe

  // Postfix
  Percent

  // ArrayOperators 
  Sum
  Avg
  Min
  Max

  // Literals
  StringLiteral

  // Number Literals
  IntegerLiteral
  FloatLiteral

  TrueLiteral
  FalseLiteral
  BooleanLiteral

  // Rest
  RParen
  Identifier
  Underscore
  Comma
  Newline
}

const keywords = [
  #("sum", Sum), #("avg", Avg), #("min", Min), #("max", Max),
  #("true", TrueLiteral), #("false", FalseLiteral),
]

pub type Token {
  Token(type_: TokenType, lexeme: String, row: Int, col: Int)
}

fn token(state: ScanState, type_: TokenType, lexeme: String) -> Token {
  Token(type_:, lexeme:, row: state.row, col: state.col)
}

pub type ScanErrorType {
  UnexpectedToken
  UnterminatedString

  /// The error returned from the scanner when a float literal is formatted with a "trailing decimal",
  /// i.e. "123.". 
  /// We want floats to have a a decimal part because it's more readable.
  TrailingDecimal
}

pub type ScanError {
  ScanError(type_: ScanErrorType, row: Int, col: Int)
}

fn error(
  state: ScanState,
  type_: ScanErrorType,
) -> Result(List(Token), ScanError) {
  Error(ScanError(type_:, row: state.row, col: state.col))
}

/// Turns a string into a list of tokens. 
pub fn scan(src: String) -> Result(List(Token), ScanError) {
  do_scan(
    src,
    ScanState(row: 1, col: 1, keywords: dict.from_list(keywords)),
    [],
  )
}

/// Previously, we scanned in src as csv to a List(List(String)).
/// I think now we should break from that and have our own expectations of source
/// that don't necessarily map onto the csv standard.
/// That means that this scanner is functionally very similar to a CSV parser.
fn do_scan(
  src: String,
  state: ScanState,
  acc: List(Token),
) -> Result(List(Token), ScanError) {
  // A helper for the extremely common case of matching a token without modifying the state
  // and appending the matched token to the accumulator.
  let match = fn(rest, lexeme, token_type) {
    do_scan(rest, state, [token(state, token_type, lexeme), ..acc])
  }

  case src {
    // BASE CASE: Done scanning, return the accumulator.
    "" -> Ok(acc |> list.reverse)

    // Double Character Tokens (we need to match these before single character tokens)
    "!=" <> rest -> match(rest, "!=", BangEqual)
    "==" <> rest -> match(rest, "==", EqualEqual)
    "<=" <> rest -> match(rest, "<=", LessEqual)
    ">=" <> rest -> match(rest, ">=", GreaterEqual)
    "||" <> rest -> match(rest, "||", Or)
    "&&" <> rest -> match(rest, "&&", And)
    "**" <> rest -> match(rest, "**", StarStar)

    // Single Character Tokens
    "=" <> rest -> match(rest, "=", Equal)
    "!" <> rest -> match(rest, "!", Bang)
    "(" <> rest -> match(rest, "(", LParen)
    ")" <> rest -> match(rest, ")", RParen)
    "+" <> rest -> match(rest, "+", Plus)
    "-" <> rest -> match(rest, "-", Minus)
    "/" <> rest -> match(rest, "/", Slash)
    "*" <> rest -> match(rest, "*", Star)
    "<" <> rest -> match(rest, "<", Less)
    ">" <> rest -> match(rest, ">", Greater)
    "_" <> rest -> match(rest, "_", Underscore)
    "$" <> rest -> match(rest, "$", Usd)
    "%" <> rest -> match(rest, "%", Percent)

    // A comma means we're moving into the next column
    "," <> rest -> {
      let new_state = ScanState(..state, col: state.col + 1)
      do_scan(rest, new_state, [token(new_state, Comma, ","), ..acc])
    }

    // A newline means we're moving into the next row
    "\n" <> rest -> {
      let new_state = ScanState(..state, row: state.row + 1, col: 1)
      do_scan(rest, new_state, [token(new_state, Newline, "\n"), ..acc])
    }

    // Numbers
    "0" as d <> rest
    | "1" as d <> rest
    | "2" as d <> rest
    | "3" as d <> rest
    | "4" as d <> rest
    | "5" as d <> rest
    | "6" as d <> rest
    | "7" as d <> rest
    | "8" as d <> rest
    | "9" as d <> rest -> {
      // First we need to parse the integer portion
      let #(whole_number, rest) = integer(rest, d)

      case rest {
        // If there's a decimal portion, we need that too
        "." <> rest -> {
          let #(decimal_portion, rest) = integer(rest, "")
          case decimal_portion {
            // If there aren't numbers following the decimal point it's a syntax error
            "" -> error(state, TrailingDecimal)

            // We got some numbers following the decimal, it's a float.
            _ -> match(rest, whole_number <> "." <> decimal_portion, FloatLiteral)
          }
        }

        // No decimal means it's an integer
        _ -> match(rest, whole_number, IntegerLiteral)
      }
    }

    _ -> {
      // Try scanning an identifier. Once you have the identifier, look it up
      // to see if it's actually a keyword
      let #(lexeme, rest) = identifier(src, "")
      case dict.get(state.keywords, lexeme) {
        // Not a keyword, scan as identifier
        Error(Nil) -> match(rest, lexeme, Identifier)

        // It's a keyword, match as that
        Ok(tt) -> match(rest, lexeme, tt)
      }
    }
  }
}

fn integer(src: String, acc: String) -> #(String, String) {
  case src {
    "0" as x <> rest
    | "1" as x <> rest
    | "2" as x <> rest
    | "3" as x <> rest
    | "4" as x <> rest
    | "5" as x <> rest
    | "6" as x <> rest
    | "7" as x <> rest
    | "8" as x <> rest
    | "9" as x <> rest -> integer(rest, x <> acc)
    _ -> #(string.reverse(acc), src)
  }
}

fn is_alpha(txt: String) -> Bool {
  txt
  |> string.to_graphemes
  |> list.fold(True, fn(acc, c) {
    string.contains("abcdefghijklmnopqrstuvwxyz", string.lowercase(c)) && acc
  })
}

fn identifier(src: String, acc: String) -> #(String, String) {
  case src |> string.pop_grapheme {
    // Only empty str left, base case
    Error(Nil) -> #(acc |> string.reverse, src)

    Ok(#(l, rest)) ->
      case is_alpha(l) {
        // Not a letter, done building the ident
        False -> #(acc |> string.reverse, src)

        // Another letter, keep building the ident
        True -> identifier(rest, l <> acc)
      }
  }
}
