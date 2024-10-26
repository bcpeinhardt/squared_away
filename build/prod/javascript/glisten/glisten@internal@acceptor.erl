-module(glisten@internal@acceptor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/2, start_pool/5]).
-export_type([acceptor_message/0, acceptor_error/0, acceptor_state/0, pool/2]).

-type acceptor_message() :: {accept_connection, glisten@socket:listen_socket()}.

-type acceptor_error() :: accept_error | handler_error | control_error.

-type acceptor_state() :: {acceptor_state,
        gleam@erlang@process:subject(acceptor_message()),
        gleam@option:option(glisten@socket:socket()),
        glisten@transport:transport()}.

-type pool(IJO, IJP) :: {pool,
        fun((glisten@internal@handler:loop_message(IJO), IJP, glisten@internal@handler:connection(IJO)) -> gleam@otp@actor:next(glisten@internal@handler:loop_message(IJO), IJP)),
        integer(),
        fun((glisten@internal@handler:connection(IJO)) -> {IJP,
            gleam@option:option(gleam@erlang@process:selector(IJO))}),
        gleam@option:option(fun((IJP) -> nil)),
        glisten@transport:transport()}.

-spec start(
    pool(any(), any()),
    gleam@erlang@process:subject(glisten@internal@listener:message())
) -> {ok, gleam@erlang@process:subject(acceptor_message())} |
    {error, gleam@otp@actor:start_error()}.
start(Pool, Listener) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Subject = gleam@erlang@process:new_subject(),
                Selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:selecting(
                        _pipe,
                        Subject,
                        fun gleam@function:identity/1
                    )
                end,
                _pipe@1 = gleam@erlang@process:try_call(
                    Listener,
                    fun(Field@0) -> {info, Field@0} end,
                    750
                ),
                _pipe@2 = gleam@result:map(
                    _pipe@1,
                    fun(State) ->
                        gleam@erlang@process:send(
                            Subject,
                            {accept_connection, erlang:element(2, State)}
                        ),
                        {ready,
                            {acceptor_state,
                                Subject,
                                none,
                                erlang:element(6, Pool)},
                            Selector}
                    end
                ),
                _pipe@3 = gleam@result:map_error(
                    _pipe@2,
                    fun(Err) ->
                        {failed,
                            <<"Failed to read listen socket: "/utf8,
                                (gleam@string:inspect(Err))/binary>>}
                    end
                ),
                gleam@result:unwrap_both(_pipe@3)
            end,
            1000,
            fun(Msg, State@1) ->
                {acceptor_state, Sender, _, _} = State@1,
                case Msg of
                    {accept_connection, Listener@1} ->
                        Res = (gleam@result:then(
                            begin
                                _pipe@4 = glisten@transport:accept(
                                    erlang:element(4, State@1),
                                    Listener@1
                                ),
                                gleam@result:replace_error(
                                    _pipe@4,
                                    accept_error
                                )
                            end,
                            fun(Sock) ->
                                gleam@result:then(
                                    begin
                                        _pipe@5 = {handler,
                                            Sock,
                                            erlang:element(2, Pool),
                                            erlang:element(4, Pool),
                                            erlang:element(5, Pool),
                                            erlang:element(6, Pool)},
                                        _pipe@6 = glisten@internal@handler:start(
                                            _pipe@5
                                        ),
                                        gleam@result:replace_error(
                                            _pipe@6,
                                            handler_error
                                        )
                                    end,
                                    fun(Start) -> _pipe@7 = Sock,
                                        _pipe@8 = glisten@transport:controlling_process(
                                            erlang:element(4, State@1),
                                            _pipe@7,
                                            gleam@erlang@process:subject_owner(
                                                Start
                                            )
                                        ),
                                        _pipe@9 = gleam@result:replace_error(
                                            _pipe@8,
                                            control_error
                                        ),
                                        gleam@result:map(
                                            _pipe@9,
                                            fun(_) ->
                                                gleam@erlang@process:send(
                                                    Start,
                                                    {internal, ready}
                                                )
                                            end
                                        ) end
                                )
                            end
                        )),
                        case Res of
                            {error, Reason} ->
                                logging:log(
                                    error,
                                    <<"Failed to accept/start handler: "/utf8,
                                        (gleam@string:inspect(Reason))/binary>>
                                ),
                                {stop,
                                    {abnormal,
                                        <<"Failed to accept/start handler"/utf8>>}};

                            _ ->
                                gleam@otp@actor:send(
                                    Sender,
                                    {accept_connection, Listener@1}
                                ),
                                gleam@otp@actor:continue(State@1)
                        end
                end
            end}
    ).

-spec start_pool(
    pool(any(), any()),
    glisten@transport:transport(),
    integer(),
    list(glisten@socket@options:tcp_option()),
    gleam@erlang@process:subject(gleam@erlang@process:subject(glisten@internal@listener:message()))
) -> {ok, gleam@erlang@process:subject(gleam@otp@supervisor:message())} |
    {error, gleam@otp@actor:start_error()}.
start_pool(Pool, Transport, Port, Options, Return) ->
    gleam@otp@supervisor:start_spec(
        {spec,
            nil,
            100,
            1,
            fun(Children) ->
                Acceptors = gleam@list:range(0, erlang:element(3, Pool)),
                _pipe@2 = gleam@otp@supervisor:add(
                    Children,
                    begin
                        _pipe@1 = gleam@otp@supervisor:worker(
                            fun(_) ->
                                _pipe = glisten@internal@listener:start(
                                    Port,
                                    Transport,
                                    Options
                                ),
                                gleam@result:map(
                                    _pipe,
                                    fun(Subj) ->
                                        gleam@erlang@process:send(Return, Subj),
                                        Subj
                                    end
                                )
                            end
                        ),
                        gleam@otp@supervisor:returning(
                            _pipe@1,
                            fun(_, Listener) -> Listener end
                        )
                    end
                ),
                gleam@list:fold(
                    Acceptors,
                    _pipe@2,
                    fun(Children@1, _) ->
                        gleam@otp@supervisor:add(
                            Children@1,
                            gleam@otp@supervisor:worker(
                                fun(Listener@1) -> start(Pool, Listener@1) end
                            )
                        )
                    end
                )
            end}
    ).
