-record(time, {
    wall_time :: integer(),
    offset :: integer(),
    timezone :: gleam@option:option(binary()),
    monotonic_time :: gleam@option:option(integer())
}).
