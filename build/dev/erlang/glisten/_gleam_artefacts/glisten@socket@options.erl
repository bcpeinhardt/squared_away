-module(glisten@socket@options).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_dict/1, merge_with_defaults/1]).
-export_type([socket_mode/0, active_state/0, tcp_option/0]).

-type socket_mode() :: binary.

-type active_state() :: once | passive | {count, integer()} | active.

-type tcp_option() :: {backlog, integer()} |
    {nodelay, boolean()} |
    {linger, {boolean(), integer()}} |
    {send_timeout, integer()} |
    {send_timeout_close, boolean()} |
    {reuseaddr, boolean()} |
    {active_mode, active_state()} |
    {mode, socket_mode()} |
    {certfile, binary()} |
    {keyfile, binary()} |
    {alpn_preferred_protocols, list(binary())} |
    inet6 |
    {buffer, integer()}.

-spec to_dict(list(tcp_option())) -> gleam@dict:dict(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()).
to_dict(Options) ->
    Opt_decoder = gleam@dynamic:tuple2(
        fun gleam@dynamic:dynamic/1,
        fun gleam@dynamic:dynamic/1
    ),
    _pipe = Options,
    _pipe@1 = gleam@list:map(_pipe, fun(Opt) -> case Opt of
                {active_mode, passive} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>), false}
                    );

                {active_mode, active} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>), true}
                    );

                {active_mode, {count, N}} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>), N}
                    );

                {active_mode, once} ->
                    gleam@dynamic:from(
                        {erlang:binary_to_atom(<<"active"/utf8>>),
                            erlang:binary_to_atom(<<"once"/utf8>>)}
                    );

                Other ->
                    gleam@dynamic:from(Other)
            end end),
    _pipe@2 = gleam@list:filter_map(_pipe@1, Opt_decoder),
    maps:from_list(_pipe@2).

-spec merge_with_defaults(list(tcp_option())) -> list(tcp_option()).
merge_with_defaults(Options) ->
    Overrides = to_dict(Options),
    _pipe = [{backlog, 1024},
        {nodelay, true},
        {send_timeout, 30000},
        {send_timeout_close, true},
        {reuseaddr, true},
        {mode, binary},
        {active_mode, passive}],
    _pipe@1 = to_dict(_pipe),
    _pipe@2 = gleam@dict:merge(_pipe@1, Overrides),
    _pipe@3 = maps:to_list(_pipe@2),
    _pipe@4 = gleam@list:map(_pipe@3, fun gleam@dynamic:from/1),
    glisten_ffi:add_inet6(_pipe@4).
