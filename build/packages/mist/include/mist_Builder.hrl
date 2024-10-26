-record(builder, {
    port :: integer(),
    handler :: fun((gleam@http@request:request(any())) -> gleam@http@response:response(any())),
    after_start :: fun((integer(), gleam@http:scheme()) -> nil)
}).
