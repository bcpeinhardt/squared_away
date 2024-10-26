import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn cell_to_the_right(input: String) -> Option(String) {
  // A cell reference is s column number and then a row number,
  // separated by and _.
  // We can get the cell to the right by splitting the col off, incrementing
  // it, then adding the row number back

  let assert Ok(#(col, row)) = string.split_once(input, "_")
  let assert Ok(col_num) = int.parse(col)
  case col_num >= 5 {
    True -> None
    False -> Some(int.to_string(col_num + 1) <> "_" <> row)
  }
}
