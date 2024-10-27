//// Grid operations are kind of a pain to get right / can produce some 
//// boilerplate around results for get operations, so I'm gonna try and extract
//// them to a module

import gleam/dict
import gleam/list
import gleam/result

// Making the type generic since we do a grid of src
// and a grid of interpreted values
pub type Grid(a) {
  Grid(inner: dict.Dict(GridKey, a), cells: List(GridKey))
}

pub opaque type GridKey {
  GridKey(row: Int, col: Int)
}

pub fn row(grid_key: GridKey) -> Int {
  grid_key.row
}

pub fn col(grid_key: GridKey) -> Int {
  grid_key.col
}

pub fn new(width: Int, height: Int, default: a) -> Grid(a) {
  let cols = list.range(1, width)
  let rows = list.range(1, height)

  let #(g, c) =
    list.fold(rows, #(dict.new(), []), fn(acc, row) {
      let #(grid, cells) = acc

      let #(g, c) =
        list.fold(cols, #(dict.new(), []), fn(acc, col) {
          let #(g, c) = acc
          #(dict.insert(g, GridKey(row:, col:), default), [
            GridKey(row:, col:),
            ..c
          ])
        })

      #(dict.merge(grid, g), list.concat([c, cells]))
    })

  Grid(g, c)
}

pub fn insert(grid: Grid(a), key: GridKey, item: a) -> Grid(a) {
  Grid(..grid, inner: dict.insert(grid.inner, key, item))
}

pub fn fold(grid: Grid(a), acc: b, do: fn(b, GridKey, a) -> b) -> b {
  dict.fold(grid.inner, acc, do)
}

pub fn map_values(grid: Grid(a), do: fn(GridKey, a) -> b) -> Grid(b) {
  Grid(inner: dict.map_values(grid.inner, do), cells: grid.cells)
}

pub fn get(grid: Grid(a), key: GridKey) -> a {
  // Because the `GridKey` is an opaque type produced
  // by creating the grid, the get operation is safe.
  let assert Ok(item) = dict.get(grid.inner, key)
  item
}

pub fn cell_to_the_right(grid: Grid(a), key: GridKey) -> Result(GridKey, Nil) {
  list.find(grid.cells, fn(k) { k.row == key.row && k.col == key.col + 1 })
}

pub fn intersect(row_cell: GridKey, col_cell: GridKey) -> Result(GridKey, Nil) {
  let GridKey(row, col_check) = row_cell
  let GridKey(row_check, col) = col_cell
  case row != row_check && col != col_check {
    False -> Error(Nil)
    True -> Ok(GridKey(row:, col:))
  }
}

pub fn to_list(grid: Grid(a)) -> List(#(GridKey, a)) {
  dict.to_list(grid.inner)
}

pub fn resize(old_grid: Grid(a), width: Int, height: Int, default: a) -> Grid(a) {
  let cols = list.range(1, width)
  let rows = list.range(1, height)

  let #(g, c) =
    list.fold(rows, #(dict.new(), []), fn(acc, row) {
      let #(grid, cells) = acc

      let #(g, c) =
        list.fold(cols, #(dict.new(), []), fn(acc, col) {
          let #(g, c) = acc
          let key = GridKey(row:, col:)
          let val = dict.get(old_grid.inner, key) |> result.unwrap(or: default)

          #(dict.insert(g, key, val), [key, ..c])
        })

      #(dict.merge(grid, g), list.concat([c, cells]))
    })

  Grid(g, c)
}
