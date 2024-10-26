-record(package, {
    name :: binary(),
    version :: binary(),
    gleam_version_constraint :: gleam@option:option(binary()),
    modules :: gleam@dict:dict(binary(), gleam@package_interface:module_())
}).
