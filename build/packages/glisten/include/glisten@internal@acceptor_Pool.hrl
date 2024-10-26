-record(pool, {
    handler :: fun((glisten@internal@handler:loop_message(any()), any(), glisten@internal@handler:connection(any())) -> gleam@otp@actor:next(glisten@internal@handler:loop_message(any()), any())),
    pool_count :: integer(),
    on_init :: fun((glisten@internal@handler:connection(any())) -> {any(),
        gleam@option:option(gleam@erlang@process:selector(any()))}),
    on_close :: gleam@option:option(fun((any()) -> nil)),
    transport :: glisten@transport:transport()
}).
