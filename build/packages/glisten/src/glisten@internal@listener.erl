-module(glisten@internal@listener).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/3]).
-export_type([message/0, state/0]).

-type message() :: {info, gleam@erlang@process:subject(state())}.

-type state() :: {state,
        glisten@socket:listen_socket(),
        integer(),
        glisten@transport:ip_address()}.

-spec start(
    integer(),
    glisten@transport:transport(),
    list(glisten@socket@options:tcp_option())
) -> {ok, gleam@erlang@process:subject(message())} |
    {error, gleam@otp@actor:start_error()}.
start(Port, Transport, Options) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() -> _pipe = glisten@transport:listen(Transport, Port, Options),
                _pipe@2 = gleam@result:then(
                    _pipe,
                    fun(Socket) ->
                        _pipe@1 = glisten@transport:sockname(Transport, Socket),
                        gleam@result:map(
                            _pipe@1,
                            fun(Info) ->
                                {state,
                                    Socket,
                                    erlang:element(2, Info),
                                    erlang:element(1, Info)}
                            end
                        )
                    end
                ),
                _pipe@3 = gleam@result:map(
                    _pipe@2,
                    fun(State) ->
                        {ready, State, gleam_erlang_ffi:new_selector()}
                    end
                ),
                _pipe@4 = gleam@result:map_error(
                    _pipe@3,
                    fun(Err) ->
                        {failed,
                            <<"Failed to start socket listener: "/utf8,
                                (gleam@string:inspect(Err))/binary>>}
                    end
                ),
                gleam@result:unwrap_both(_pipe@4) end,
            5000,
            fun(Msg, State@1) -> case Msg of
                    {info, Caller} ->
                        gleam@erlang@process:send(Caller, State@1),
                        gleam@otp@actor:continue(State@1)
                end end}
    ).
