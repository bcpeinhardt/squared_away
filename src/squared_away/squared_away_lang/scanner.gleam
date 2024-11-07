import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import bigi

import squared_away/squared_away_lang/scanner/scan_error
import squared_away/squared_away_lang/scanner/token

pub fn scan(src: String) -> Result(List(token.Token), scan_error.ScanError) {
  case string.trim(src) {
    // No tokens will become an empty type
    "" -> Ok([])

    // Boolean literal
    "TRUE" -> Ok([token.TrueToken])
    "FALSE" -> Ok([token.FalseToken])

    // A formual starts with an = sign
    "=" <> rest -> do_scan(rest |> string.trim_left, [])
    "$" <> rest -> case parse_usd_literal(rest) {
      Error(e) -> Error(e)
      Ok(#(n, "")) -> Ok([token.UsdLiteral(cents: n)])
      _ -> Error(scan_error.ScanError("Unexpected content after $ literal. If you're typing a formula, be sure to add an equal sign in front."))
    }

    txt -> {
      // We need to try and parse the text as a number literal
      case float.parse(txt) {
        Ok(f) -> Ok([token.FloatLiteral(f)])
        Error(_) ->
          case int.parse(txt) {
            Ok(i) -> Ok([token.IntegerLiteral(i)])

            // If it's not a Bool, Float, Int, Usd, or Percent Literal, and it doesn't
            // start with a = sign, it's a LabelDef
            Error(_) -> {
              case string.ends_with(txt, "%") {
                True -> {
                  // There should be an integer between 0 and 100 inclusive in front
                  let percent = string.drop_right(txt, 1)
                  case int.parse(percent) {
                     Ok(p) if p <= 100 && p >= 0 -> Ok([token.PercentLiteral(p)])
                     Ok(_) | Error(_) -> Error(scan_error.ScanError("Percent literal must be an integer b/w 0 and 100 inclusive."))
                  }
                }
                False -> case parse_identifier(txt, "") {
                Ok(#(ident, "")) -> Ok([token.LabelDef(ident)])
                _ ->
                  Error(scan_error.ScanError(
                    "Not a label definition, boolean, float, or integer. Are you possibly missing an `=` sign?",
                  ))
              }
              }

              
            }
          }
      }
    }
  }
}

fn parse_usd_literal(
  src: String,
) -> Result(#(bigi.BigInt, String), scan_error.ScanError) {
  case parse_integer_text(src, "") {
    Error(_) ->
      Error(scan_error.ScanError(
        "Expected usd literal (a $ optionally followed by a number with two decimal places)",
      ))
    Ok(#(dollars, rest)) -> {
      let assert Ok(dollars) = bigi.from_string(dollars)
      case rest {
        "." <> rest ->
          case parse_integer(rest, "") {
            Error(_) ->
              Error(scan_error.ScanError(
                "Expected 2 decimal places following `.` character in usd literal",
              ))
            Ok(#(cents, rest)) if cents > 0 && cents < 100 ->
              Ok(#(bigi.multiply(dollars, bigi.from_int(100) |> bigi.add(bigi.from_int(cents))), rest))
            Ok(#(_, _)) ->
              Error(scan_error.ScanError(
                "Usd literal must have zero or two decimal places.",
              ))
          }
        _ -> Ok(#(bigi.multiply(dollars, bigi.from_int(100)), rest))
      }
    }
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
    "mustbe" <> rest -> do_scan(string.trim_left(rest), [token.MustBe, ..acc])
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
    "_" <> rest -> do_scan(string.trim_left(rest), [token.Underscore, ..acc])
    "sum" <> rest ->
      do_scan(string.trim_left(rest), [token.BuiltinSum(option.None), ..acc])
    "$" <> rest -> case parse_usd_literal(rest) {
      Error(e) -> Error(e)
      Ok(#(cents, rest)) -> do_scan(string.trim_left(rest), [token.UsdLiteral(cents:), ..acc])
    }
    _ -> {
      case parse_integer(src, "") {
        Ok(#(n, rest)) -> {
          // Might be a float
          case rest {
            "." <> rest -> {
              use #(m, rest) <- result.try(
                parse_integer(rest, "")
                |> result.replace_error(scan_error.ScanError(
                  "Expected more digits after the decimal.",
                )),
              )
              let assert Ok(f) =
                float.parse(int.to_string(n) <> "." <> int.to_string(m))
              do_scan(string.trim_left(rest), [token.FloatLiteral(f), ..acc])
            }
            "%" <> rest if n >= 0 && n <= 100 -> {
              do_scan(string.trim_left(rest), [token.PercentLiteral(percent: n), ..acc])
            }
            _ ->
              do_scan(string.trim_left(rest), [token.IntegerLiteral(n), ..acc])
          }
        }

        Error(_) -> {
          case parse_identifier(src, "") {
            Error(Nil) ->
              Error(scan_error.ScanError(
                "Could not understand provided txt: " <> src,
              ))
            Ok(#(ident, rest)) ->
              do_scan(string.trim_left(rest), [token.Label(ident), ..acc])
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
    | "Z" as l <> rest
    | "a" as l <> rest
    | "b" as l <> rest
    | "c" as l <> rest
    | "d" as l <> rest
    | "e" as l <> rest
    | "f" as l <> rest
    | "g" as l <> rest
    | "h" as l <> rest
    | "i" as l <> rest
    | "j" as l <> rest
    | "k" as l <> rest
    | "l" as l <> rest
    | "m" as l <> rest
    | "n" as l <> rest
    | "o" as l <> rest
    | "p" as l <> rest
    | "q" as l <> rest
    | "r" as l <> rest
    | "s" as l <> rest
    | "t" as l <> rest
    | "u" as l <> rest
    | "v" as l <> rest
    | "w" as l <> rest
    | "x" as l <> rest
    | "y" as l <> rest
    | "z" as l <> rest -> parse_identifier(rest, acc <> l)
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

fn parse_integer_text(src: String, acc: String) -> Result(#(String, String), Nil) {
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
    | "0" as x <> rest -> parse_integer_text(rest, acc <> x)
    _ if acc != "" -> Ok(#(acc, src))
    _ -> Error(Nil)
  }
}