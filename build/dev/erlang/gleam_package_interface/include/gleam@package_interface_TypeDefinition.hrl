-record(type_definition, {
    documentation :: gleam@option:option(binary()),
    deprecation :: gleam@option:option(gleam@package_interface:deprecation()),
    parameters :: integer(),
    constructors :: list(gleam@package_interface:type_constructor())
}).
