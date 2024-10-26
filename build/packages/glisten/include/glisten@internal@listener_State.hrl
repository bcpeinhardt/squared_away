-record(state, {
    listen_socket :: glisten@socket:listen_socket(),
    port :: integer(),
    ip_address :: glisten@transport:ip_address()
}).
