import gleam/bool
import gleam/iterator
import gleam/option
import gleam/order

/// returns a function that can be used to create a range
///
/// ## Examples
///
/// ```gleam
/// > let range =
/// >  create(
/// >    validate: fn(a) { string.length(a) == 1 },
/// >    negate_step: fn(s) { -1 * s },
/// >    add: fn(a: String, b: Int) {
/// >      let assert [code] = string.to_utf_codepoints(a)
/// >      let int_code = string.utf_codepoint_to_int(code)
/// >      let new_int_code = int_code + b
/// >      let assert Ok(new_code) = string.utf_codepoint(new_int_code)
/// >      string.from_utf_codepoints([new_code])
/// >    },
/// >    compare: string.compare,
/// >  )
///
/// > range("ab", "e", 1)
/// Error(Nil)
///
/// > let assert Ok(a_to_e) = range("a", "e", 1)
/// > a_to_e |> iterator.to_list
/// ["a", "b", "c", "d", "e"]
/// 
/// > let assert Ok(z_to_p) = range("z", "p", 1)
/// > z_to_p |> iterator.to_list
/// ["z", "y", "x", "w", "v", "u", "t", "s", "r", "q", "p"]
///
/// > let assert Ok(z_to_p) = range("z", "p", -2)
/// > z_to_p |> iterator.to_list
/// ["z", "x", "v", "t", "r", "p"]
///
/// > let assert Ok(z_to_p) = range("z", "p", 3)
/// > z_to_p |> iterator.to_list
/// ["z", "w", "t", "q"]
/// ```
///
///
/// ```gleam
/// > let range =
/// >    create(
/// >      validate: fn(_) { True },
/// >      negate_step: fn(s) { -1.0 *. s },
/// >      add: fn(a, b) { a +. b },
/// >      compare: float.compare,
/// >    )
///
/// > let assert Ok(weird_step_case) = range(1.25, 4.5, -0.5)
/// > weird_step_case |> iterator.to_list
/// [1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 4.25]
/// 
/// > let assert Ok(single_item_case) = range(1.25, 1.25, -0.25)
/// > single_item_case |> iterator.to_list
/// [1.25]
///
/// > let assert Ok(zero_step_case) = range(2.5, 5.0, 0)
/// > zero_step_case |> iterator.to_list
/// [2.5]
/// ```
///
pub fn create(
  validate validate: fn(item_type) -> Bool,
  negate_step negate_step: fn(step_type) -> step_type,
  add add: fn(item_type, step_type) -> item_type,
  compare compare: fn(item_type, item_type) -> order.Order,
) -> fn(item_type, item_type, step_type) ->
  Result(iterator.Iterator(item_type), Nil) {
  let adjust_step = fn(a, b, step) -> Result(
    option.Option(#(Direction, step_type)),
    Nil,
  ) {
    let negated_step = negate_step(step)

    case
      compare(a, b),
      compare(a, add(a, step)),
      compare(a, add(a, negated_step))
    {
      order.Eq, _, _ -> Ok(option.None)
      _, order.Eq, order.Eq -> Ok(option.None)

      order.Lt, order.Lt, _ -> Ok(option.Some(#(Forward, step)))
      order.Lt, _, order.Lt -> Ok(option.Some(#(Forward, negated_step)))
      order.Lt, _, _ -> Error(Nil)

      order.Gt, order.Gt, _ -> Ok(option.Some(#(Backward, step)))
      order.Gt, _, order.Gt -> Ok(option.Some(#(Backward, negated_step)))
      order.Gt, _, _ -> Error(Nil)
    }
  }

  fn(a: item_type, b: item_type, s: step_type) {
    use <- bool.guard(!validate(a) || !validate(b), Error(Nil))

    case adjust_step(a, b, s) {
      Ok(option.Some(#(direction, step))) ->
        Ok(
          iterator.unfold(a, fn(current) {
            case compare(current, b), direction {
              order.Gt, Forward -> iterator.Done
              order.Lt, Backward -> iterator.Done
              _, _ -> iterator.Next(current, add(current, step))
            }
          }),
        )
      Ok(option.None) -> Ok(iterator.once(fn() { a }))
      Error(Nil) -> Error(Nil)
    }
  }
}

/// returns a function that can be used to create an infinite range
///
/// should be used carefully because careless use of infinite iterators could crash your app
///
/// ## Examples
///
/// ```gleam
/// > let range =
/// >  create_infinite(
/// >    validate: fn(a) { string.length(a) == 1 },
/// >    add: fn(a: String, b: Int) {
/// >      let assert [code] = string.to_utf_codepoints(a)
/// >      let int_code = string.utf_codepoint_to_int(code)
/// >      let new_int_code = int_code + b
/// >      let assert Ok(new_code) = string.utf_codepoint(new_int_code)
/// >      string.from_utf_codepoints([new_code])
/// >    },
/// >    compare: string.compare,
/// >   )
///
/// > let assert Ok(from_a) = range("a", 1)
/// > from_a |> iterator.take(26) |> iterator.to_list
/// ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
///   "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
/// ```
pub fn create_infinite(
  validate validate: fn(item_type) -> Bool,
  add add: fn(item_type, step_type) -> item_type,
  compare compare: fn(item_type, item_type) -> order.Order,
) -> fn(item_type, step_type) -> Result(iterator.Iterator(item_type), Nil) {
  let is_step_zero = fn(a, s) -> Bool {
    case compare(a, add(a, s)) {
      order.Eq -> True
      _ -> False
    }
  }

  fn(a: item_type, s: step_type) {
    use <- bool.guard(!validate(a), Error(Nil))

    use <- bool.guard(
      is_step_zero(a, s),
      iterator.once(fn() { a })
        |> Ok,
    )

    iterator.unfold(a, fn(current) { iterator.Next(current, add(current, s)) })
    |> Ok
  }
}

type Direction {
  Forward
  Backward
}
