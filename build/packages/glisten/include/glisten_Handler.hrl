-record(handler, {
    on_init :: fun((glisten:connection(any())) -> {any(),
        gleam@option:option(gleam@erlang@process:selector(any()))}),
    loop :: fun((glisten:message(any()), any(), glisten:connection(any())) -> gleam@otp@actor:next(glisten:message(any()), any())),
    on_close :: gleam@option:option(fun((any()) -> nil)),
    pool_size :: integer(),
    http2_support :: boolean()
}).
