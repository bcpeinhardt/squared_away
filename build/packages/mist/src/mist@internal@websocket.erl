-module(mist@internal@websocket).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([initialize_connection/6]).
-export_type([valid_message/1, websocket_message/1, websocket_connection/0, handler_message/1, websocket_state/1]).

-type valid_message(OQP) :: {socket_message, bitstring()} |
    socket_closed_message |
    {user_message, OQP}.

-type websocket_message(OQQ) :: {valid, valid_message(OQQ)} | invalid.

-type websocket_connection() :: {websocket_connection,
        glisten@socket:socket(),
        glisten@transport:transport(),
        gleam@option:option(gramps@websocket@compression:context())}.

-type handler_message(OQR) :: {internal, gramps@websocket:frame()} | {user, OQR}.

-type websocket_state(OQS) :: {websocket_state,
        bitstring(),
        OQS,
        gleam@option:option(gramps@websocket@compression:compression())}.

-spec message_selector() -> gleam@erlang@process:selector(websocket_message(any())).
message_selector() ->
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@6 = gleam@erlang@process:selecting_record3(
        _pipe,
        erlang:binary_to_atom(<<"tcp"/utf8>>),
        fun(_, Data) -> _pipe@1 = Data,
            _pipe@2 = gleam@dynamic:bit_array(_pipe@1),
            _pipe@3 = gleam@result:replace_error(_pipe@2, nil),
            _pipe@4 = gleam@result:map(
                _pipe@3,
                fun(Field@0) -> {socket_message, Field@0} end
            ),
            _pipe@5 = gleam@result:map(
                _pipe@4,
                fun(Field@0) -> {valid, Field@0} end
            ),
            gleam@result:unwrap(_pipe@5, invalid) end
    ),
    _pipe@12 = gleam@erlang@process:selecting_record3(
        _pipe@6,
        erlang:binary_to_atom(<<"ssl"/utf8>>),
        fun(_, Data@1) -> _pipe@7 = Data@1,
            _pipe@8 = gleam@dynamic:bit_array(_pipe@7),
            _pipe@9 = gleam@result:replace_error(_pipe@8, nil),
            _pipe@10 = gleam@result:map(
                _pipe@9,
                fun(Field@0) -> {socket_message, Field@0} end
            ),
            _pipe@11 = gleam@result:map(
                _pipe@10,
                fun(Field@0) -> {valid, Field@0} end
            ),
            gleam@result:unwrap(_pipe@11, invalid) end
    ),
    _pipe@13 = gleam@erlang@process:selecting_record2(
        _pipe@12,
        erlang:binary_to_atom(<<"ssl_closed"/utf8>>),
        fun(_) -> {valid, socket_closed_message} end
    ),
    gleam@erlang@process:selecting_record2(
        _pipe@13,
        erlang:binary_to_atom(<<"tcp_closed"/utf8>>),
        fun(_) -> {valid, socket_closed_message} end
    ).

-spec get_messages(
    bitstring(),
    list(gramps@websocket:parsed_frame()),
    gleam@option:option(gramps@websocket@compression:context())
) -> {list(gramps@websocket:parsed_frame()), bitstring()}.
get_messages(Data, Frames, Context) ->
    case gramps@websocket:frame_from_message(Data, Context) of
        {ok, {Frame, <<>>}} ->
            {lists:reverse([Frame | Frames]), <<>>};

        {ok, {Frame@1, Rest}} ->
            get_messages(Rest, [Frame@1 | Frames], Context);

        {error, {need_more_data, Rest@1}} ->
            {lists:reverse(Frames), Rest@1};

        {error, invalid_frame} ->
            {lists:reverse(Frames), Data}
    end.

-spec set_active(glisten@transport:transport(), glisten@socket:socket()) -> nil.
set_active(Transport, Socket) ->
    _assert_subject = glisten@transport:set_opts(
        Transport,
        Socket,
        [{active_mode, once}]
    ),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/websocket"/utf8>>,
                        function => <<"set_active"/utf8>>,
                        line => 355})
    end,
    nil.

-spec map_user_selector(gleam@option:option(gleam@erlang@process:selector(OSA))) -> gleam@option:option(gleam@erlang@process:selector(websocket_message(OSA))).
map_user_selector(Selector) ->
    gleam@option:map(
        Selector,
        fun(_capture) ->
            gleam_erlang_ffi:map_selector(
                _capture,
                fun(Msg) -> {valid, {user_message, Msg}} end
            )
        end
    ).

-spec apply_frames(
    list(gramps@websocket:frame()),
    fun((ORQ, websocket_connection(), handler_message(ORR)) -> gleam@otp@actor:next(ORR, ORQ)),
    websocket_connection(),
    gleam@otp@actor:next(websocket_message(ORR), ORQ),
    fun((ORQ) -> nil)
) -> gleam@otp@actor:next(websocket_message(ORR), ORQ).
apply_frames(Frames, Handler, Connection, Next, On_close) ->
    case {Frames, Next} of
        {_, {stop, Reason}} ->
            {stop, Reason};

        {[], Next@1} ->
            set_active(
                erlang:element(3, Connection),
                erlang:element(2, Connection)
            ),
            Next@1;

        {[{control, {close_frame, _, _}} = Frame | _], {continue, State, _}} ->
            _ = glisten@transport:send(
                erlang:element(3, Connection),
                erlang:element(2, Connection),
                gramps@websocket:frame_to_bytes_builder(Frame, none)
            ),
            On_close(State),
            {stop, normal};

        {[{control, {ping_frame, Length, Payload}} | _], {continue, State@1, _}} ->
            _pipe = glisten@transport:send(
                erlang:element(3, Connection),
                erlang:element(2, Connection),
                gramps@websocket:frame_to_bytes_builder(
                    {control, {pong_frame, Length, Payload}},
                    none
                )
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(_) ->
                    set_active(
                        erlang:element(3, Connection),
                        erlang:element(2, Connection)
                    ),
                    gleam@otp@actor:continue(State@1)
                end
            ),
            gleam@result:lazy_unwrap(
                _pipe@1,
                fun() ->
                    On_close(State@1),
                    {stop, {abnormal, <<"Failed to send pong frame"/utf8>>}}
                end
            );

        {[Frame@1 | Rest], {continue, State@2, Prev_selector}} ->
            case gleam_erlang_ffi:rescue(
                fun() -> Handler(State@2, Connection, {internal, Frame@1}) end
            ) of
                {ok, {continue, State@3, Selector}} ->
                    Next_selector = begin
                        _pipe@2 = Selector,
                        _pipe@3 = map_user_selector(_pipe@2),
                        _pipe@4 = gleam@option:'or'(_pipe@3, Prev_selector),
                        gleam@option:map(
                            _pipe@4,
                            fun(With_user) ->
                                gleam_erlang_ffi:merge_selector(
                                    message_selector(),
                                    With_user
                                )
                            end
                        )
                    end,
                    apply_frames(
                        Rest,
                        Handler,
                        Connection,
                        {continue, State@3, Next_selector},
                        On_close
                    );

                {ok, {stop, Reason@1}} ->
                    On_close(State@2),
                    {stop, Reason@1};

                {error, Reason@2} ->
                    logging:log(
                        error,
                        <<"Caught error in websocket handler: "/utf8,
                            (gleam@erlang:format(Reason@2))/binary>>
                    ),
                    On_close(State@2),
                    {stop,
                        {abnormal, <<"Crash in user websocket handler"/utf8>>}}
            end
    end.

-spec initialize_connection(
    fun((websocket_connection()) -> {ORB,
        gleam@option:option(gleam@erlang@process:selector(ORC))}),
    fun((ORB) -> nil),
    fun((ORB, websocket_connection(), handler_message(ORC)) -> gleam@otp@actor:next(ORC, ORB)),
    glisten@socket:socket(),
    glisten@transport:transport(),
    list(binary())
) -> {ok, gleam@erlang@process:subject(websocket_message(ORC))} | {error, nil}.
initialize_connection(On_init, On_close, Handler, Socket, Transport, Extensions) ->
    _pipe@11 = gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Compression = case gramps@websocket:has_deflate(Extensions) of
                    true ->
                        {some, gramps@websocket@compression:init()};

                    false ->
                        none
                end,
                Connection = {websocket_connection,
                    Socket,
                    Transport,
                    gleam@option:map(
                        Compression,
                        fun(Compression@1) ->
                            erlang:element(3, Compression@1)
                        end
                    )},
                {Initial_state, User_selector} = On_init(Connection),
                Selector = case User_selector of
                    {some, User_selector@1} ->
                        _pipe = User_selector@1,
                        _pipe@1 = gleam_erlang_ffi:map_selector(
                            _pipe,
                            fun(Field@0) -> {user_message, Field@0} end
                        ),
                        _pipe@2 = gleam_erlang_ffi:map_selector(
                            _pipe@1,
                            fun(Field@0) -> {valid, Field@0} end
                        ),
                        gleam_erlang_ffi:merge_selector(
                            _pipe@2,
                            message_selector()
                        );

                    _ ->
                        message_selector()
                end,
                {ready,
                    {websocket_state, <<>>, Initial_state, Compression},
                    Selector}
            end,
            500,
            fun(Msg, State) ->
                Connection@1 = {websocket_connection,
                    Socket,
                    Transport,
                    gleam@option:map(
                        erlang:element(4, State),
                        fun(Compression@2) ->
                            erlang:element(3, Compression@2)
                        end
                    )},
                case Msg of
                    {valid, {socket_message, Data}} ->
                        {Frames, Rest} = get_messages(
                            <<(erlang:element(2, State))/bitstring,
                                Data/bitstring>>,
                            [],
                            gleam@option:map(
                                erlang:element(4, State),
                                fun(Compression@3) ->
                                    erlang:element(2, Compression@3)
                                end
                            )
                        ),
                        _pipe@3 = Frames,
                        _pipe@4 = gramps@websocket:aggregate_frames(
                            _pipe@3,
                            none,
                            []
                        ),
                        _pipe@5 = gleam@result:map(
                            _pipe@4,
                            fun(Frames@1) ->
                                Next = apply_frames(
                                    Frames@1,
                                    Handler,
                                    Connection@1,
                                    gleam@otp@actor:continue(
                                        erlang:element(3, State)
                                    ),
                                    On_close
                                ),
                                case Next of
                                    {continue, User_state, Selector@1} ->
                                        {continue,
                                            erlang:setelement(
                                                3,
                                                erlang:setelement(
                                                    2,
                                                    State,
                                                    Rest
                                                ),
                                                User_state
                                            ),
                                            Selector@1};

                                    {stop, Reason} ->
                                        _ = gleam@option:map(
                                            erlang:element(4, State),
                                            fun(Contexts) ->
                                                zlib:close(
                                                    erlang:element(3, Contexts)
                                                ),
                                                zlib:close(
                                                    erlang:element(2, Contexts)
                                                )
                                            end
                                        ),
                                        {stop, Reason}
                                end
                            end
                        ),
                        gleam@result:lazy_unwrap(
                            _pipe@5,
                            fun() ->
                                logging:log(
                                    error,
                                    <<"Received a malformed WebSocket frame"/utf8>>
                                ),
                                On_close(erlang:element(3, State)),
                                _ = gleam@option:map(
                                    erlang:element(4, State),
                                    fun(Contexts@1) ->
                                        zlib:close(
                                            erlang:element(3, Contexts@1)
                                        ),
                                        zlib:close(
                                            erlang:element(2, Contexts@1)
                                        )
                                    end
                                ),
                                {stop,
                                    {abnormal,
                                        <<"WebSocket received a malformed message"/utf8>>}}
                            end
                        );

                    {valid, {user_message, Msg@1}} ->
                        _pipe@6 = gleam_erlang_ffi:rescue(
                            fun() ->
                                Handler(
                                    erlang:element(3, State),
                                    Connection@1,
                                    {user, Msg@1}
                                )
                            end
                        ),
                        _pipe@9 = gleam@result:map(
                            _pipe@6,
                            fun(Cont) -> case Cont of
                                    {continue, User_state@1, Selector@2} ->
                                        Selector@3 = begin
                                            _pipe@7 = Selector@2,
                                            _pipe@8 = map_user_selector(_pipe@7),
                                            gleam@option:map(
                                                _pipe@8,
                                                fun(With_user) ->
                                                    gleam_erlang_ffi:merge_selector(
                                                        message_selector(),
                                                        With_user
                                                    )
                                                end
                                            )
                                        end,
                                        {continue,
                                            erlang:setelement(
                                                3,
                                                State,
                                                User_state@1
                                            ),
                                            Selector@3};

                                    {stop, Reason@1} ->
                                        _ = gleam@option:map(
                                            erlang:element(4, State),
                                            fun(Contexts@2) ->
                                                zlib:close(
                                                    erlang:element(
                                                        3,
                                                        Contexts@2
                                                    )
                                                ),
                                                zlib:close(
                                                    erlang:element(
                                                        2,
                                                        Contexts@2
                                                    )
                                                )
                                            end
                                        ),
                                        On_close(erlang:element(3, State)),
                                        {stop, Reason@1}
                                end end
                        ),
                        _pipe@10 = gleam@result:map_error(
                            _pipe@9,
                            fun(Err) ->
                                logging:log(
                                    error,
                                    <<"Caught error in websocket handler: "/utf8,
                                        (gleam@erlang:format(Err))/binary>>
                                )
                            end
                        ),
                        gleam@result:lazy_unwrap(
                            _pipe@10,
                            fun() ->
                                _ = gleam@option:map(
                                    erlang:element(4, State),
                                    fun(Contexts@3) ->
                                        zlib:close(
                                            erlang:element(3, Contexts@3)
                                        ),
                                        zlib:close(
                                            erlang:element(2, Contexts@3)
                                        )
                                    end
                                ),
                                On_close(erlang:element(3, State)),
                                {stop,
                                    {abnormal,
                                        <<"Crash in user websocket handler"/utf8>>}}
                            end
                        );

                    {valid, socket_closed_message} ->
                        _ = gleam@option:map(
                            erlang:element(4, State),
                            fun(Contexts@4) ->
                                zlib:close(erlang:element(3, Contexts@4)),
                                zlib:close(erlang:element(2, Contexts@4))
                            end
                        ),
                        On_close(erlang:element(3, State)),
                        {stop, normal};

                    invalid ->
                        logging:log(
                            error,
                            <<"Received a malformed WebSocket frame"/utf8>>
                        ),
                        _ = gleam@option:map(
                            erlang:element(4, State),
                            fun(Contexts@5) ->
                                zlib:close(erlang:element(3, Contexts@5)),
                                zlib:close(erlang:element(2, Contexts@5))
                            end
                        ),
                        On_close(erlang:element(3, State)),
                        {stop,
                            {abnormal,
                                <<"WebSocket received a malformed message"/utf8>>}}
                end
            end}
    ),
    _pipe@12 = gleam@result:replace_error(_pipe@11, nil),
    _pipe@13 = gleam@result:map(
        _pipe@12,
        fun(Subj) ->
            Websocket_pid = gleam@erlang@process:subject_owner(Subj),
            _assert_subject = glisten@transport:controlling_process(
                Transport,
                Socket,
                Websocket_pid
            ),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"initialize_connection"/utf8>>,
                                line => 256})
            end,
            set_active(Transport, Socket),
            Subj
        end
    ),
    gleam@result:replace_error(_pipe@13, nil).
