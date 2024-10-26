-record(builder, {
    frames :: list(binary()),
    text :: binary(),
    colour :: fun((binary()) -> binary())
}).
