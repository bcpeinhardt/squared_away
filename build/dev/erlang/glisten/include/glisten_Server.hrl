-record(server, {
    listener :: gleam@erlang@process:subject(glisten@internal@listener:message()),
    supervisor :: gleam@erlang@process:subject(gleam@otp@supervisor:message()),
    transport :: glisten@transport:transport()
}).
