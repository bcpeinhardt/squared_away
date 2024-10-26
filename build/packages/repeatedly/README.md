# repeatedly

Call a function repeatedly with a delay between each call.

[![Package Version](https://img.shields.io/hexpm/v/repeatedly)](https://hex.pm/packages/repeatedly)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/repeatedly/)

Works on both Erlang and JavaScript runtimes!

```sh
gleam add repeatedly
```
```gleam
import repeatedly

pub fn main() {
  // Print every 500ms
  let repeater = repeatedly.call(500, Nil, fn(_state, i) {
    io.println("Call number " <> int.to_string(i))
  })

  // And at some point later, stop it
  repeatedly.stop(repeater)
}
```

Further documentation can be found at <https://hexdocs.pm/repeatedly>.

