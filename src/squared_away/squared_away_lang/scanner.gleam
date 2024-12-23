import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import squared_away/squared_away_lang/util/rational

import squared_away/squared_away_lang/scanner/scan_error
import squared_away/squared_away_lang/scanner/token

fn get_str(
  chars: String,
  acc: String,
) -> Result(#(String, String), scan_error.ScanError) {
  case string.pop_grapheme(chars) {
    // Error means we reached the end of the string without finding a closing quote
    Error(_) -> Error(scan_error.ScanError("Missing closing quote for string"))
    Ok(#(c, rest)) -> {
      case c {
        "\"" -> Ok(#(acc, rest))
        _ -> get_str(rest, c <> acc)
      }
    }
  }
}

pub fn scan(src: String) -> Result(List(token.Token), scan_error.ScanError) {
  case string.trim(src) {
    // No tokens will become an empty type
    "" -> Ok([])

    // A formula starts with an = sign
    "=" <> rest -> do_scan(rest |> string.trim_left, [])

    // Boolean literal
    "TRUE" -> Ok([token.TrueToken])
    "FALSE" -> Ok([token.FalseToken])

    // USD Literal
    "$" <> rest -> {
      use #(n, rest) <- result.try(
        rational.from_string(rest)
        |> result.replace_error(scan_error.ScanError(
          "Could not parse USD literal",
        )),
      )
      use <- bool.guard(
        rest != "",
        Error(scan_error.ScanError("Found extra content after USD literal")),
      )
      Ok([token.UsdLiteral(dollars: n)])
    }

    // String literal
    // For now, we'll make a string literal be wrapped in quotes.
    // We're not adding escape for backslashes or anything yet because I'm not convinced we'll
    // keep this format, as most spreadsheets opt for unquoted text in cells.
    "\"" <> rest ->
      case get_str(rest, "") {
        Error(e) -> Error(e)
        Ok(#(str_content, "")) ->
          Ok([token.StringLiteral(str_content |> string.reverse)])
        Ok(_) ->
          Error(scan_error.ScanError("Found extra content after string literal"))
      }

    txt -> {
      // Try an identifier
      case parse_identifier(txt, "") {
        Ok(#(ident, rest)) ->
          case rest {
            "" -> Ok([token.LabelDef(ident)])
            _ -> Error(scan_error.ScanError("Unexpected content: " <> rest))
          }
        Error(_) -> {
          // If the text ends with a %, try parsing it as a rational
          case string.ends_with(txt, "%") {
            True -> {
              use #(p, rest) <- result.try(
                rational.from_string(txt)
                |> result.replace_error(scan_error.ScanError(
                  "Expected valid number before % sign.",
                )),
              )
              use <- bool.guard(
                rest != "%",
                Error(scan_error.ScanError(
                  "Expected valid number before % literal",
                )),
              )
              Ok([
                token.PercentLiteral(rational.divide(p, rational.from_int(100))),
              ])
            }
            False -> {
              // We need to try and parse the text as a number literal
              case float.parse(txt) {
                Ok(f) -> Ok([token.FloatLiteral(f)])
                Error(_) ->
                  case int.parse(txt) {
                    Ok(i) -> Ok([token.IntegerLiteral(i)])

                    Error(_) -> Error(scan_error.ScanError("duh"))
                  }
              }
            }
          }
        }
      }
    }
  }
}

fn do_scan(
  src: String,
  acc: List(token.Token),
) -> Result(List(token.Token), scan_error.ScanError) {
  case src {
    // Base Case
    "" -> Ok(acc |> list.reverse)

    // Builtins
    "mustbe" <> rest -> do_scan(string.trim_left(rest), [token.MustBe, ..acc])
    "^+" <> rest ->
      do_scan(string.trim_left(rest), [token.BuiltinSum(option.None), ..acc])
    "avg" <> rest ->
      do_scan(string.trim_left(rest), [token.BuiltinAvg(option.None), ..acc])
    "min" <> rest -> do_scan(string.trim_left(rest), [token.Minimum, ..acc])
    "max" <> rest -> do_scan(string.trim_left(rest), [token.Maximum, ..acc])

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

    // If it's not one of those, it's an identifier
    _ -> {
      use #(ident, rest) <- result.try(
        parse_identifier(src, "")
        |> result.replace_error(scan_error.ScanError(
          "Could not understand provided txt: " <> src,
        )),
      )
      do_scan(string.trim_left(rest), [token.Label(ident), ..acc])
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
        // Meaning we called this on something that didnt start with a letter
        "" -> Error(Nil)
        _ -> {
          Ok(#(acc, src))
        }
      }
    }
  }
}
