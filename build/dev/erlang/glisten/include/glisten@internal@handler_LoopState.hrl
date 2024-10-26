-record(loop_state, {
    client_ip :: {ok, {glisten@transport:ip_address(), integer()}} |
        {error, nil},
    socket :: glisten@socket:socket(),
    sender :: gleam@erlang@process:subject(glisten@internal@handler:message(any())),
    transport :: glisten@transport:transport(),
    data :: any()
}).
