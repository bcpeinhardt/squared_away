import birdie
import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import squared_away_lang as lang
import squared_away_lang/error
import squared_away_lang/interpreter/value

pub fn main() {
  gleeunit.main()
}

fn empty_grid() -> dict.Dict(String, String) {
  let cols = "ABCDEF" |> string.to_graphemes
  let rows = list.range(1, 6)

  list.fold(cols, dict.new(), fn(grid, c) {
    list.fold(rows, dict.new(), fn(partial_grid, r) {
      let key = c <> int.to_string(r)
      partial_grid |> dict.insert(key, "")
    })
    |> dict.merge(grid)
  })
}

fn print_grid_values(
  grid: dict.Dict(String, Result(value.Value, error.CompileError)),
  keys: List(String),
) -> String {
  use acc, key, val <- dict.fold(grid, "")
  case list.contains(keys, key) {
    False -> acc
    True ->
      case val {
        Ok(v) -> acc <> key <> ": " <> value.value_to_string(v) <> "\n"
        Error(e) -> acc <> key <> ": " <> string.inspect(e)
      }
  }
}

// gleeunit test functions end in `_test`
pub fn basic_label_usage_test() {
  let grid =
    empty_grid()
    |> dict.insert("A1", "X")
    |> dict.insert("B1", "=4")
    |> dict.insert("B2", "=X")

  let res = {
    let scanned = lang.scan_grid(grid)
    let parsed = lang.parse_grid(scanned)
    let typechecked = lang.typecheck_grid(parsed)
    lang.interpret_grid(typechecked)
  }

  print_grid_values(res, ["A1", "B1", "B2"])
  |> birdie.snap(title: "Basic Label Usage")
}

pub fn parse_cross_ref_test() {
  let grid =
    empty_grid()
    |> dict.insert("A2", "Ben")
    |> dict.insert("B1", "Height")
    |> dict.insert("B2", "=4")
    |> dict.insert("C2", "=Ben_Height")

  let res = {
    let scanned = lang.scan_grid(grid)
    let parsed = lang.parse_grid(scanned)
    let typechecked = lang.typecheck_grid(parsed)
    lang.interpret_grid(typechecked)
  }

  print_grid_values(res, ["A2", "B1", "B2", "C2"])
  |> birdie.snap(title: "Parse Cross Reference")
}
