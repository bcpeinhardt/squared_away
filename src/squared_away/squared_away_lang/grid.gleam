//// Grid operations are kind of a pain to get right / can produce some 
//// boilerplate around results for get operations, so I'm gonna try and extract
//// them to a module

import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gsv

// Making the type generic since we do a grid of src
// and a grid of interpreted values
pub type Grid(a) {
  Grid(inner: dict.Dict(GridKey, a), cells: List(GridKey))
}

pub opaque type GridKey {
  GridKey(row: Int, col: Int)
}

pub fn to_string(key: GridKey) -> String {
  let GridKey(row, col) = key
  "(" <> int.to_string(row) <> "," <> int.to_string(col) <> ")"
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

      #(dict.merge(grid, g), list.flatten([c, cells]))
    })

  Grid(g, c)
}

pub fn insert(grid: Grid(a), key: GridKey, item: a) -> Grid(a) {
  Grid(..grid, inner: dict.insert(grid.inner, key, item))
}

pub fn find(grid: Grid(a), item: a) -> Result(GridKey, Nil) {
  grid.inner
  |> dict.to_list
  |> list.find_map(fn(i) {
    let #(k, i) = i
    case i == item {
      False -> Error(Nil)
      True -> Ok(k)
    }
  })
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

pub fn cell_underneath(grid: Grid(a), key: GridKey) -> Result(GridKey, Nil) {
  list.find(grid.cells, fn(k) { k.row == key.row + 1 && k.col == key.col })
}

pub fn cell_above(grid: Grid(a), key: GridKey) -> Result(GridKey, Nil) {
  list.find(grid.cells, fn(k) { k.row + 1 == key.row && k.col == key.col })
}

pub fn cell_to_the_left(grid: Grid(a), key: GridKey) -> Result(GridKey, Nil) {
  list.find(grid.cells, fn(k) { k.row == key.row && k.col + 1 == key.col })
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

pub fn src_csv(grid: Grid(String)) -> String {
  to_list(grid)
  |> list.sort(fn(c1, c2) {
    let #(GridKey(r1, c1), _) = c1
    let #(GridKey(r2, c2), _) = c2
    case int.compare(r1, r2) {
      order.Eq -> int.compare(c1, c2)
      _ as x -> x
    }
  })
  |> list.chunk(fn(c) {
    let #(GridKey(r, _), _) = c
    r
  })
  |> list.map(list.map(_, fn(c) {
    let #(_, src) = c
    src
  }))
  |> gsv.from_lists(",", gsv.Unix)
}

pub fn from_src_csv(src: String, width: Int, height: Int) -> Grid(String) {
  // Filter down the content to just the appropriate rows and columns 
  let assert Ok(src) = gsv.to_lists(src)
  let inner =
    list.take(src, height)
    |> list.map(list.take(_, width))
    |> list.index_map(fn(src_row, row_index) {
      list.index_map(src_row, fn(cell_content, col_index) {
        #(GridKey(row_index + 1, col_index + 1), cell_content)
      })
    })
    |> list.flatten
    |> dict.from_list

  let cells = dict.keys(inner)
  Grid(inner, cells)
}
