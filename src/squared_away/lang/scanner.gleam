import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string

import squared_away/lang/scanner/scan_error
import squared_away/lang/scanner/token

pub fn scan(src: String) -> Result(List(token.Token), scan_error.ScanError) {
  case string.trim(src) {
    "" -> Ok([])
    "=" <> rest -> do_scan(rest |> string.trim_left, [])
    _ -> Ok([token.LabelDef(src, "")])
  }
}

fn do_scan(
  src: String,
  acc: List(token.Token),
) -> Result(List(token.Token), scan_error.ScanError) {
  case src {
    "" -> Ok(acc |> list.reverse)
    "TRUE" <> rest -> do_scan(string.trim_left(rest), [token.TrueToken, ..acc])
    "FALSE" <> rest ->
      do_scan(string.trim_left(rest), [token.FalseToken, ..acc])
    "&&" <> rest -> do_scan(string.trim_left(rest), [token.And, ..acc])
    "||" <> rest -> do_scan(string.trim_left(rest), [token.Or, ..acc])
    "**" <> rest -> do_scan(string.trim_left(rest), [token.StarStar, ..acc])
    "==" <> rest -> do_scan(string.trim_left(rest), [token.EqualEqual, ..acc])
    "!=" <> rest -> do_scan(string.trim_left(rest), [token.BangEqual, ..acc])
    "<=" <> rest -> do_scan(string.trim_left(rest), [token.LessEqual, ..acc])
    ">=" <> rest -> do_scan(string.trim_left(rest), [token.GreaterEqual, ..acc])
    "+" <> rest -> do_scan(string.trim_left(rest), [token.Plus, ..acc])
    "-" <> rest -> do_scan(string.trim_left(rest), [token.Minus, ..acc])
    "*" <> rest -> do_scan(string.trim_left(rest), [token.Star, ..acc])
    "/" <> rest -> do_scan(string.trim_left(rest), [token.Div, ..acc])
    "=" <> rest -> do_scan(string.trim_left(rest), [token.Equal, ..acc])
    "!" <> rest -> do_scan(string.trim_left(rest), [token.Bang, ..acc])
    "<" <> rest -> do_scan(string.trim_left(rest), [token.Less, ..acc])
    ">" <> rest -> do_scan(string.trim_left(rest), [token.Greater, ..acc])
    "(" <> rest -> do_scan(string.trim_left(rest), [token.LParen, ..acc])
    ")" <> rest -> do_scan(string.trim_left(rest), [token.RParen, ..acc])
    _ -> {
      case parse_integer(src, "") {
        Ok(#(n, rest)) -> {
          // Might be a float
          case rest {
            "." <> rest -> {
              use #(m, rest) <- result.try(
                parse_integer(rest, "")
                |> result.replace_error(scan_error.ScanError),
              )
              let assert Ok(f) =
                float.parse(int.to_string(n) <> "." <> int.to_string(m))
              do_scan(string.trim_left(rest), [token.FloatLiteral(f), ..acc])
            }
            _ ->
              do_scan(string.trim_left(rest), [token.IntegerLiteral(n), ..acc])
          }
        }

        Error(_) -> {
          case parse_cell_ref(src, "") {
            Ok(#(cell_ref, rest)) ->
              do_scan(string.trim_left(rest), [
                token.CellReference(cell_ref),
                ..acc
              ])
            _ -> {
              case parse_identifier(src, "") {
                Error(Nil) -> Error(scan_error.ScanError)
                Ok(#(ident, rest)) ->
                  do_scan(string.trim_left(rest), [token.Label(ident), ..acc])
              }
            }
          }
        }
      }
    }
  }
}

fn parse_identifier(src: String, acc: String) -> Result(#(String, String), Nil) {
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
    | "Z" as l <> rest -> parse_identifier(rest, acc <> l)
    _ -> {
      case acc {
        // Meaning we called this on something that didnt start with a capital letter
        "" -> Error(Nil)
        _ -> {
          Ok(#(acc, src))
        }
      }
    }
  }
}

fn parse_cell_ref(src: String, acc: String) -> Result(#(String, String), Nil) {
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
    | "Z" as l <> rest -> parse_cell_ref(rest, acc <> l)
    _ -> {
      case acc {
        // Meaning we called this on something that didnt start with a capital letter
        "" -> Error(Nil)
        _ -> {
          use #(n, rest) <- result.try(parse_integer(src, ""))
          Ok(#(acc <> int.to_string(n), rest))
        }
      }
    }
  }
}

fn parse_integer(src: String, acc: String) -> Result(#(Int, String), Nil) {
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
    | "0" as x <> rest -> parse_integer(rest, acc <> x)
    _ -> int.parse(acc) |> result.map(fn(n) { #(n, src) })
  }
}
