![ranger](https://raw.githubusercontent.com/massivefermion/ranger/main/banner.png)

[![Package Version](https://img.shields.io/hexpm/v/ranger)](https://hex.pm/packages/ranger)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/ranger/)

# ranger

create ranges over any type

## <img width=64 src="https://raw.githubusercontent.com/massivefermion/ranger/main/icon.png"> Quick start

```sh
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```

## <img width=64 src="https://raw.githubusercontent.com/massivefermion/ranger/main/icon.png"> Installation

This package can be added to your Gleam project:

```sh
gleam add ranger
```

and its documentation can be found at <https://hexdocs.pm/ranger>.

## <img width=64 src="https://raw.githubusercontent.com/massivefermion/ranger/main/icon.png"> Usage

```gleam
import ranger

pub fn main() {
  let range =
    ranger.create(
      validate: fn(_) { True },
      negate_step: fn(s) { -1.0 *. s },
      add: fn(a, b) { a +. b },
      compare: float.compare,
    )

  let assert Ok(z_to_p) = range("z", "p", 1)
  z_to_p
  |> iterator.to_list
}
```
