-record(server, {
    supervisor :: gleam@erlang@process:subject(gleam@otp@supervisor:message()),
    port :: integer(),
    ip_address :: mist:ip_address()
}).
