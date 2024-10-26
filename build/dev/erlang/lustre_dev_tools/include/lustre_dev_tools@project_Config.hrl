-record(config, {
    name :: binary(),
    version :: binary(),
    toml :: gleam@dict:dict(binary(), tom:toml())
}).
