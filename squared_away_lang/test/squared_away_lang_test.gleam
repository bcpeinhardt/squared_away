import birdie
import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import pprint
import squared_away_lang as lang

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

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let grid =
    empty_grid()
    |> dict.insert("A1", "X")
    |> dict.insert("A2", "=4")
    |> dict.insert("B2", "=X")

  let res = {
    let scanned = lang.scan_grid(grid)
    let parsed = lang.parse_grid(scanned)
    let typechecked = lang.typecheck_grid(parsed)
    lang.interpret_grid(typechecked)
  }

  pprint.format(res) |> birdie.snap(title: "Basic Label Usage")
}
