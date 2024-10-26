-record(handler, {
    socket :: glisten@socket:socket(),
    loop :: fun((glisten@internal@handler:loop_message(any()), any(), glisten@internal@handler:connection(any())) -> gleam@otp@actor:next(glisten@internal@handler:loop_message(any()), any())),
    on_init :: fun((glisten@internal@handler:connection(any())) -> {any(),
        gleam@option:option(gleam@erlang@process:selector(any()))}),
    on_close :: gleam@option:option(fun((any()) -> nil)),
    transport :: glisten@transport:transport()
}).
