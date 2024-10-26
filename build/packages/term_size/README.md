# term_size

Retrieve the terminal's size in rows and columns.

[![Package Version](https://img.shields.io/hexpm/v/term_size)](https://hex.pm/packages/term_size)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/term_size/)

✨ This package works on all targets!

```sh
gleam add term_size
```

```gleam
import gleam/io
import gleam/string
import term_size

pub fn main() {
  let assert Ok(columns) = term_size.columns()

  string.repeat("-", columns)
  |> io.println
}
```

Further documentation can be found at <https://hexdocs.pm/term_size>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
