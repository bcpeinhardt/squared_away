# gleam_package_interface

[![Package Version](https://img.shields.io/hexpm/v/gleam_package_interface)](https://hex.pm/packages/gleam_package_interface)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleam_package_interface/)
![Supported targets](https://img.shields.io/badge/supports-all_targets-ffaff3)

## Installation

Add `gleam_package_interface` to your Gleam project:

```sh
gleam add gleam_package_interface
```

## What's a package interface?

Whenever you build your project's documentation with `gleam docs build`, the
Gleam compiler will also produce a handy json file
`./build/dev/docs/<your package>/package-interface.json`
containing data about your package: that's the package interface.

It has all public information your package exposes to the outside world: type
definitions, type aliases, public functions and constants — each annotated with
its own type and with its documentation.

> You can also have the compiler build the package interface using the
> `gleam export package-interface` command.

Let's have a look at a small example. Imagine you have a module called `wibble`
with the following definition:

```gleam
/// Documentation!
pub fn wibbler(label n: Int) -> Int {
  todo
}
```

The resulting package interface will look something like this (some keys where
omitted to keep the example short):

```json
{
  "package": "your package",
  "modules": {
    "wibble": {
      "functions": {
        "wibbler": {
          "documentation": "Documentation!\n",
          "parameters": [
            {
              "label": "label",
              "type": {
                "kind": "named",
                "package": "",
                "module": "gleam",
                "name": "Int"
              }
            }
          ],
          "return": {
            "kind": "named",
            "package": "",
            "module": "gleam",
            "name": "Int"
          }
        }
      }
    }
  }
}
```

To get a proper feel of the structure of the generated package interface you can
have a look at this
[package's types](https://hexdocs.pm/gleam_package_interface/).

## Usage

This package provides Gleam types to describe a package interface and a
`decoder` to decode the json package interface into a Gleam value.

```gleam
// gleam add gleam_json
// gleam add simplifile
import gleam/json
import gleam/package_interface
import simplifile

pub fn main() {
  let assert Ok(json) = simplifile.read("path to the package interface")
  let assert Ok(interface) = json.decode(json, using: package_interface.decoder)
  todo as "now you can use the package interface however you want!"
}
```
