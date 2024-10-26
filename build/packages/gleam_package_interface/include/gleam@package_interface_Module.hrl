-record(module, {
    documentation :: list(binary()),
    type_aliases :: gleam@dict:dict(binary(), gleam@package_interface:type_alias()),
    types :: gleam@dict:dict(binary(), gleam@package_interface:type_definition()),
    constants :: gleam@dict:dict(binary(), gleam@package_interface:constant()),
    functions :: gleam@dict:dict(binary(), gleam@package_interface:function_())
}).
