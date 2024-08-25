import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Token {
  /// +, addition op for integers
  Plus
  /// -, subtraction op for integers
  Minus
  /// *
  Star
  /// /
  Div
  /// **
  StarStar
  /// =
  Equal
  /// ==
  EqualEqual
  /// !=
  BangEqual
  /// !
  Bang
  /// <
  Less
  /// <=
  LessEqual
  /// >
  Greater
  /// >=
  GreaterEqual
  /// 6, 73
  IntegerLiteral(n: Int)
  /// 1.0, 6.87
  FloatLiteral(f: Float)
  /// True
  TrueToken
  /// False
  FalseToken
  /// &&
  And
  /// ||
  Or
  /// (
  LParen
  /// ) 
  RParen
  /// Cell Reference A3, XX532
  CellReference(key: String)
  /// Anything not starting with an = in a cell is a string literal
  StringLiteral(String)
}

pub type ScanError {
  ScanError
}

pub fn scan(src: String) -> Result(List(Token), ScanError) {
  case string.trim(src) {
    "" -> Ok([])
    "=" <> rest -> do_scan(rest |> string.trim_left, [])
    _ -> Ok([StringLiteral(src)])
  }
}

fn do_scan(src: String, acc: List(Token)) -> Result(List(Token), ScanError) {
  case src {
    "" -> Ok(acc |> list.reverse)
    "TRUE" <> rest -> do_scan(string.trim_left(rest), [TrueToken, ..acc])
    "FALSE" <> rest -> do_scan(string.trim_left(rest), [FalseToken, ..acc])
    "&&" <> rest -> do_scan(string.trim_left(rest), [And, ..acc])
    "||" <> rest -> do_scan(string.trim_left(rest), [Or, ..acc])
    "**" <> rest -> do_scan(string.trim_left(rest), [StarStar, ..acc])
    "==" <> rest -> do_scan(string.trim_left(rest), [EqualEqual, ..acc])
    "!=" <> rest -> do_scan(string.trim_left(rest), [BangEqual, ..acc])
    "<=" <> rest -> do_scan(string.trim_left(rest), [LessEqual, ..acc])
    ">=" <> rest -> do_scan(string.trim_left(rest), [GreaterEqual, ..acc])
    "+" <> rest -> do_scan(string.trim_left(rest), [Plus, ..acc])
    "-" <> rest -> do_scan(string.trim_left(rest), [Minus, ..acc])
    "*" <> rest -> do_scan(string.trim_left(rest), [Star, ..acc])
    "/" <> rest -> do_scan(string.trim_left(rest), [Div, ..acc])
    "=" <> rest -> do_scan(string.trim_left(rest), [Equal, ..acc])
    "!" <> rest -> do_scan(string.trim_left(rest), [Bang, ..acc])
    "<" <> rest -> do_scan(string.trim_left(rest), [Less, ..acc])
    ">" <> rest -> do_scan(string.trim_left(rest), [Greater, ..acc])
    "(" <> rest -> do_scan(string.trim_left(rest), [LParen, ..acc])
    ")" <> rest -> do_scan(string.trim_left(rest), [RParen, ..acc])
    _ -> {
      case maybe_parse_integer(src, "") {
        Some(#(n, rest)) -> {
          // Might be a float
          case rest {
            "." <> rest -> {
              case maybe_parse_integer(rest, "") {
                Some(#(m, rest)) -> {
                  let assert Ok(f) =
                    float.parse(int.to_string(n) <> "." <> int.to_string(m))
                  do_scan(string.trim_left(rest), [FloatLiteral(f), ..acc])
                }
                None -> Error(ScanError)
              }
            }
            _ -> do_scan(string.trim_left(rest), [IntegerLiteral(n), ..acc])
          }
        }

        None ->
          case maybe_parse_cell_ref(src, "") {
            Some(#(cell_ref, rest)) ->
              do_scan(string.trim_left(rest), [CellReference(cell_ref), ..acc])
            None -> Error(ScanError)
          }
      }
    }
  }
}

fn maybe_parse_cell_ref(src: String, acc: String) -> Option(#(String, String)) {
  // A cell reference is a string of characters followed by a string of numbers (aka an int),
  // so we can reuse the integer parsing at a slight runtime cost for now
  case src {
    "A" as l <> rest
    | "B" as l <> rest
    | "C" as l <> rest
    | "D" as l <> rest
    | "E" as l <> rest
    | "F" as l <> rest
    | "G" as l <> rest
    | "H" as l <> rest
    | "I" as l <> rest
    | "J" as l <> rest
    | "K" as l <> rest
    | "L" as l <> rest
    | "M" as l <> rest
    | "N" as l <> rest
    | "O" as l <> rest
    | "P" as l <> rest
    | "Q" as l <> rest
    | "R" as l <> rest
    | "S" as l <> rest
    | "T" as l <> rest
    | "U" as l <> rest
    | "V" as l <> rest
    | "W" as l <> rest
    | "X" as l <> rest
    | "Y" as l <> rest
    | "Z" as l <> rest -> maybe_parse_cell_ref(rest, acc <> l)
    _ -> {
      case acc {
        // Meaning we called this on something that didnt start with a capital letter
        "" -> None
        _ -> {
          case maybe_parse_integer(src, "") {
            Some(#(n, rest)) -> Some(#(acc <> int.to_string(n), rest))
            None -> None
          }
        }
      }
    }
  }
}

fn maybe_parse_integer(src: String, acc: String) -> Option(#(Int, String)) {
  case src {
    "1" as x <> rest
    | "2" as x <> rest
    | "3" as x <> rest
    | "4" as x <> rest
    | "5" as x <> rest
    | "6" as x <> rest
    | "7" as x <> rest
    | "8" as x <> rest
    | "9" as x <> rest
    | "0" as x <> rest -> maybe_parse_integer(rest, acc <> x)
    _ -> {
      case int.parse(acc) {
        Ok(x) -> Some(#(x, src))
        _ -> None
      }
    }
  }
}
