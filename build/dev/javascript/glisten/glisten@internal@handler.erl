-module(glisten@internal@handler).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/1]).
-export_type([internal_message/0, message/1, loop_message/1, loop_state/2, connection/1, handler/2]).

-type internal_message() :: close |
    ready |
    {receive_message, bitstring()} |
    ssl_closed |
    tcp_closed.

-type message(HXP) :: {internal, internal_message()} | {user, HXP}.

-type loop_message(HXQ) :: {packet, bitstring()} | {custom, HXQ}.

-type loop_state(HXR, HXS) :: {loop_state,
        {ok, {glisten@transport:ip_address(), integer()}} | {error, nil},
        glisten@socket:socket(),
        gleam@erlang@process:subject(message(HXR)),
        glisten@transport:transport(),
        HXS}.

-type connection(HXT) :: {connection,
        {ok, {glisten@transport:ip_address(), integer()}} | {error, nil},
        glisten@socket:socket(),
        glisten@transport:transport(),
        gleam@erlang@process:subject(message(HXT))}.

-type handler(HXU, HXV) :: {handler,
        glisten@socket:socket(),
        fun((loop_message(HXU), HXV, connection(HXU)) -> gleam@otp@actor:next(loop_message(HXU), HXV)),
        fun((connection(HXU)) -> {HXV,
            gleam@option:option(gleam@erlang@process:selector(HXU))}),
        gleam@option:option(fun((HXV) -> nil)),
        glisten@transport:transport()}.

-spec start(handler(HYF, any())) -> {ok,
        gleam@erlang@process:subject(message(HYF))} |
    {error, gleam@otp@actor:start_error()}.
start(Handler) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Subject = gleam@erlang@process:new_subject(),
                Client_ip = begin
                    _pipe = glisten@transport:peername(
                        erlang:element(6, Handler),
                        erlang:element(2, Handler)
                    ),
                    gleam@result:nil_error(_pipe)
                end,
                Connection = {connection,
                    Client_ip,
                    erlang:element(2, Handler),
                    erlang:element(6, Handler),
                    Subject},
                {Initial_state, User_selector} = (erlang:element(4, Handler))(
                    Connection
                ),
                Selector = begin
                    _pipe@1 = gleam_erlang_ffi:new_selector(),
                    _pipe@5 = gleam@erlang@process:selecting_record3(
                        _pipe@1,
                        erlang:binary_to_atom(<<"tcp"/utf8>>),
                        fun(_, Data) -> _pipe@2 = Data,
                            _pipe@3 = gleam@dynamic:bit_array(_pipe@2),
                            _pipe@4 = gleam@result:unwrap(_pipe@3, <<>>),
                            {receive_message, _pipe@4} end
                    ),
                    _pipe@9 = gleam@erlang@process:selecting_record3(
                        _pipe@5,
                        erlang:binary_to_atom(<<"ssl"/utf8>>),
                        fun(_, Data@1) -> _pipe@6 = Data@1,
                            _pipe@7 = gleam@dynamic:bit_array(_pipe@6),
                            _pipe@8 = gleam@result:unwrap(_pipe@7, <<>>),
                            {receive_message, _pipe@8} end
                    ),
                    _pipe@10 = gleam@erlang@process:selecting_record2(
                        _pipe@9,
                        erlang:binary_to_atom(<<"ssl_closed"/utf8>>),
                        fun(_) -> ssl_closed end
                    ),
                    _pipe@11 = gleam@erlang@process:selecting_record2(
                        _pipe@10,
                        erlang:binary_to_atom(<<"tcp_closed"/utf8>>),
                        fun(_) -> tcp_closed end
                    ),
                    _pipe@12 = gleam_erlang_ffi:map_selector(
                        _pipe@11,
                        fun(Field@0) -> {internal, Field@0} end
                    ),
                    gleam@erlang@process:selecting(
                        _pipe@12,
                        Subject,
                        fun gleam@function:identity/1
                    )
                end,
                Selector@1 = case User_selector of
                    {some, Sel} ->
                        _pipe@13 = Sel,
                        _pipe@14 = gleam_erlang_ffi:map_selector(
                            _pipe@13,
                            fun(Field@0) -> {user, Field@0} end
                        ),
                        gleam_erlang_ffi:merge_selector(Selector, _pipe@14);

                    _ ->
                        Selector
                end,
                {ready,
                    {loop_state,
                        Client_ip,
                        erlang:element(2, Handler),
                        Subject,
                        erlang:element(6, Handler),
                        Initial_state},
                    Selector@1}
            end,
            1000,
            fun(Msg, State) ->
                Connection@1 = {connection,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(5, State),
                    erlang:element(4, State)},
                case Msg of
                    {internal, tcp_closed} ->
                        case glisten@transport:close(
                            erlang:element(5, State),
                            erlang:element(3, State)
                        ) of
                            {ok, nil} ->
                                _ = case erlang:element(5, Handler) of
                                    {some, On_close} ->
                                        On_close(erlang:element(6, State));

                                    _ ->
                                        nil
                                end,
                                {stop, normal};

                            {error, Err} ->
                                {stop, {abnormal, gleam@string:inspect(Err)}}
                        end;

                    {internal, ssl_closed} ->
                        case glisten@transport:close(
                            erlang:element(5, State),
                            erlang:element(3, State)
                        ) of
                            {ok, nil} ->
                                _ = case erlang:element(5, Handler) of
                                    {some, On_close} ->
                                        On_close(erlang:element(6, State));

                                    _ ->
                                        nil
                                end,
                                {stop, normal};

                            {error, Err} ->
                                {stop, {abnormal, gleam@string:inspect(Err)}}
                        end;

                    {internal, close} ->
                        case glisten@transport:close(
                            erlang:element(5, State),
                            erlang:element(3, State)
                        ) of
                            {ok, nil} ->
                                _ = case erlang:element(5, Handler) of
                                    {some, On_close} ->
                                        On_close(erlang:element(6, State));

                                    _ ->
                                        nil
                                end,
                                {stop, normal};

                            {error, Err} ->
                                {stop, {abnormal, gleam@string:inspect(Err)}}
                        end;

                    {internal, ready} ->
                        _pipe@15 = erlang:element(3, State),
                        _pipe@16 = glisten@transport:handshake(
                            erlang:element(5, State),
                            _pipe@15
                        ),
                        _pipe@17 = gleam@result:replace_error(
                            _pipe@16,
                            <<"Failed to handshake socket"/utf8>>
                        ),
                        _pipe@19 = gleam@result:then(
                            _pipe@17,
                            fun(_) ->
                                _ = begin
                                    _pipe@18 = glisten@transport:set_buffer_size(
                                        erlang:element(5, State),
                                        erlang:element(3, State)
                                    ),
                                    gleam@result:map_error(
                                        _pipe@18,
                                        fun(Err@1) ->
                                            logging:log(
                                                warning,
                                                <<"Failed to read `recbuf` size, using default: "/utf8,
                                                    (gleam@string:inspect(Err@1))/binary>>
                                            )
                                        end
                                    )
                                end,
                                {ok, nil}
                            end
                        ),
                        _pipe@21 = gleam@result:then(
                            _pipe@19,
                            fun(_) ->
                                _pipe@20 = glisten@transport:set_opts(
                                    erlang:element(5, State),
                                    erlang:element(3, State),
                                    [{active_mode, once}]
                                ),
                                gleam@result:replace_error(
                                    _pipe@20,
                                    <<"Failed to set socket active"/utf8>>
                                )
                            end
                        ),
                        _pipe@22 = gleam@result:replace(
                            _pipe@21,
                            gleam@otp@actor:continue(State)
                        ),
                        _pipe@23 = gleam@result:map_error(
                            _pipe@22,
                            fun(Reason) -> {stop, {abnormal, Reason}} end
                        ),
                        gleam@result:unwrap_both(_pipe@23);

                    {user, Msg@1} ->
                        Msg@2 = {custom, Msg@1},
                        Res = gleam_erlang_ffi:rescue(
                            fun() ->
                                (erlang:element(3, Handler))(
                                    Msg@2,
                                    erlang:element(6, State),
                                    Connection@1
                                )
                            end
                        ),
                        case Res of
                            {ok, {continue, Next_state, _}} ->
                                _assert_subject = glisten@transport:set_opts(
                                    erlang:element(5, State),
                                    erlang:element(3, State),
                                    [{active_mode, once}]
                                ),
                                {ok, nil} = case _assert_subject of
                                    {ok, nil} -> _assert_subject;
                                    _assert_fail ->
                                        erlang:error(
                                                #{gleam_error => let_assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail,
                                                    module => <<"glisten/internal/handler"/utf8>>,
                                                    function => <<"start"/utf8>>,
                                                    line => 193}
                                            )
                                end,
                                gleam@otp@actor:continue(
                                    erlang:setelement(6, State, Next_state)
                                );

                            {ok, {stop, Reason@1}} ->
                                {stop, Reason@1};

                            {error, Reason@2} ->
                                logging:log(
                                    error,
                                    <<"Caught error in user handler: "/utf8,
                                        (gleam@string:inspect(Reason@2))/binary>>
                                ),
                                gleam@otp@actor:continue(State)
                        end;

                    {internal, {receive_message, Msg@3}} ->
                        Msg@4 = {packet, Msg@3},
                        Res@1 = gleam_erlang_ffi:rescue(
                            fun() ->
                                (erlang:element(3, Handler))(
                                    Msg@4,
                                    erlang:element(6, State),
                                    Connection@1
                                )
                            end
                        ),
                        case Res@1 of
                            {ok, {continue, Next_state@1, _}} ->
                                _assert_subject@1 = glisten@transport:set_opts(
                                    erlang:element(5, State),
                                    erlang:element(3, State),
                                    [{active_mode, once}]
                                ),
                                {ok, nil} = case _assert_subject@1 of
                                    {ok, nil} -> _assert_subject@1;
                                    _assert_fail@1 ->
                                        erlang:error(
                                                #{gleam_error => let_assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail@1,
                                                    module => <<"glisten/internal/handler"/utf8>>,
                                                    function => <<"start"/utf8>>,
                                                    line => 214}
                                            )
                                end,
                                gleam@otp@actor:continue(
                                    erlang:setelement(6, State, Next_state@1)
                                );

                            {ok, {stop, Reason@3}} ->
                                {stop, Reason@3};

                            {error, Reason@4} ->
                                logging:log(
                                    error,
                                    <<"Caught error in user handler: "/utf8,
                                        (gleam@string:inspect(Reason@4))/binary>>
                                ),
                                gleam@otp@actor:continue(State)
                        end
                end
            end}
    ).
