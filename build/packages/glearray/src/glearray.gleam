import gleam/iterator.{type Iterator}

/// Arrays are ordered sequences of elements, similar to lists.
///
/// Like everything in Gleam, arrays are immutable.
/// As opposed to linked lists, arrays store their elements in a contiguous
/// slice of memory, therefore allowing very fast indexed access.
///
/// Modifying an array however takes linear time and memory because it requires
/// copying the entire array.
///
/// ### Implementation
///
/// Arrays are represented as tuples when compiled to Erlang, and JavaScript
/// arrays when compiled to JavaScript.
///
/// Also note that this library has no connection to Erlang's
/// [`array`](https://www.erlang.org/doc/man/array.html) module, which
/// implements a tree structure for efficient reading and writing.
///
pub type Array(a)

/// Returns an empty array.
///
/// ## Examples
///
/// ```gleam
/// > new()
/// from_list([])
/// ```
///
@external(erlang, "glearray_ffi", "new")
@external(javascript, "./glearray_ffi.mjs", "newArray")
pub fn new() -> Array(a)

/// Converts a list to an array.
///
@external(erlang, "erlang", "list_to_tuple")
@external(javascript, "./glearray_ffi.mjs", "fromList")
pub fn from_list(list: List(a)) -> Array(a)

/// Converts an array to a list.
///
@external(erlang, "erlang", "tuple_to_list")
@external(javascript, "./gleam.mjs", "toList")
pub fn to_list(array: Array(a)) -> List(a)

/// Returns the number of elements in the array.
///
/// ## Performance
///
/// This function is very efficient and runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// > length(new())
/// 0
/// ```
///
/// ```gleam
/// > from_list([8, 0, 0]) |> length
/// 3
/// ```
///
@external(erlang, "erlang", "tuple_size")
@external(javascript, "./glearray_ffi.mjs", "arrayLength")
pub fn length(of array: Array(a)) -> Int

/// Returns the element at the specified index, starting from 0.
///
/// `Error(Nil)` is returned if `index` is less than 0 or greater than
/// or equal to `length(array)`.
///
/// ## Performance
///
/// This function is very efficient and runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// > from_list([5, 6, 7]) |> get(1)
/// Ok(6)
/// ```
///
/// ```gleam
/// > from_list([5, 6, 7]) |> get(3)
/// Error(Nil)
/// ```
///
pub fn get(in array: Array(a), at index: Int) -> Result(a, Nil) {
  case is_valid_index(array, index) {
    True -> Ok(do_get(array, index))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "get")
@external(javascript, "./glearray_ffi.mjs", "get")
fn do_get(array: Array(a), index: Int) -> a

/// Replaces the element at the given index with `value`.
///
/// This function cannot extend an array and returns `Error(Nil)` if `index` is
/// not valid.
/// See also [`copy_insert`](#copy_insert) and [`copy_push`](#copy_push).
///
/// ## Performance
///
/// This function has to copy the entire array, making it very inefficient
/// especially for larger arrays.
///
/// ## Examples
///
/// ```gleam
/// > from_list(["a", "b", "c"]) |> copy_set(1, "x")
/// Ok(from_list(["a", "x", "c"]))
/// ```
///
/// ```gleam
/// > from_list(["a", "b", "c"]) |> copy_set(3, "x")
/// Error(Nil)
/// ```
///
pub fn copy_set(
  in array: Array(a),
  at index: Int,
  value value: a,
) -> Result(Array(a), Nil) {
  case is_valid_index(array, index) {
    True -> Ok(do_set(array, index, value))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "set")
@external(javascript, "./glearray_ffi.mjs", "set")
fn do_set(array: Array(a), index: Int, value: a) -> Array(a)

fn is_valid_index(array: Array(a), index: Int) -> Bool {
  index >= 0 && index < length(array)
}

/// Adds a single element to the back of the given array.
///
/// ## Performance
///
/// This function has to copy the entire array, making it very inefficient
/// especially for larger arrays.
///
/// ## Examples
///
/// ```gleam
/// > new() |> copy_push(1) |> copy_push(2) |> to_list
/// [1, 2]
/// ```
///
@external(erlang, "erlang", "append_element")
@external(javascript, "./glearray_ffi.mjs", "push")
pub fn copy_push(onto array: Array(a), value value: a) -> Array(a)

/// Inserts an element into the array at the given index.
///
/// All following elements are shifted to the right, having their index
/// incremented by one.
///
/// `Error(Nil)` is returned if the index is less than 0 or greater than
/// `length(array)`.
/// If the index is equal to `length(array)`, this function behaves like
/// [`copy_push`](#copy_push).
///
/// ## Performance
///
/// This function has to copy the entire array, making it very inefficient
/// especially for larger arrays.
///
/// ## Examples
///
/// ```gleam
/// > from_list(["a", "b"]) |> copy_insert(0, "c")
/// Ok(from_list(["c", "a", "b"]))
/// ```
///
/// ```gleam
/// > from_list(["a", "b"]) |> copy_insert(1, "c")
/// Ok(from_list(["a", "c", "b"]))
/// ```
///
/// ```gleam
/// > from_list(["a", "b"]) |> copy_insert(2, "c")
/// Ok(from_list(["a", "b", "c"]))
/// ```
///
/// ```gleam
/// > from_list(["a", "b"]) |> copy_insert(3, "c")
/// Error(Nil)
/// ```
///
pub fn copy_insert(
  into array: Array(a),
  at index: Int,
  value value: a,
) -> Result(Array(a), Nil) {
  case index >= 0 && index <= length(array) {
    True -> Ok(do_insert(array, index, value))
    False -> Error(Nil)
  }
}

@external(erlang, "glearray_ffi", "insert")
@external(javascript, "./glearray_ffi.mjs", "insert")
fn do_insert(array: Array(a), index: Int, value: a) -> Array(a)

/// Returns an [`Iterator`](https://hexdocs.pm/gleam_stdlib/gleam/iterator.html#Iterator)
/// yielding each element in this array.
///
/// ## Examples
///
/// ```gleam
/// > from_list(["a", "b", "c"])
/// > |> iterate
/// > |> iterator.map(string.uppercase)
/// > |> iterator.to_list
/// ["A", "B", "C"]
/// ```
///
pub fn iterate(array: Array(a)) -> Iterator(a) {
  use index <- iterator.unfold(from: 0)
  case get(array, index) {
    Ok(element) -> iterator.Next(element, index + 1)
    Error(_) -> iterator.Done
  }
}
