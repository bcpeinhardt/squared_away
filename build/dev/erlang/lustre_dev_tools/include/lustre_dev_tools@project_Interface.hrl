-record(interface, {
    name :: binary(),
    version :: binary(),
    modules :: gleam@dict:dict(binary(), lustre_dev_tools@project:module_())
}).
