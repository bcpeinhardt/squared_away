-record(function, {
    documentation :: gleam@option:option(binary()),
    deprecation :: gleam@option:option(gleam@package_interface:deprecation()),
    implementations :: gleam@package_interface:implementations(),
    parameters :: list(gleam@package_interface:parameter()),
    return :: gleam@package_interface:type()
}).
