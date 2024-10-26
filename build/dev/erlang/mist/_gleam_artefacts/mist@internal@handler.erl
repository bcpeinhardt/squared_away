-module(mist@internal@handler).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new_state/1, init/1, with_func/1]).
-export_type([handler_error/0, state/0]).

-type handler_error() :: {invalid_request, mist@internal@http:decode_error()} |
    not_found.

-type state() :: {http1,
        mist@internal@http@handler:state(),
        gleam@erlang@process:subject(mist@internal@http2@handler:message())} |
    {http2, mist@internal@http2@handler:state()}.

-spec new_state(
    gleam@erlang@process:subject(mist@internal@http2@handler:message())
) -> state().
new_state(Subj) ->
    {http1, mist@internal@http@handler:initial_state(), Subj}.

-spec init(any()) -> {state(),
    gleam@option:option(gleam@erlang@process:selector(mist@internal@http2@handler:message()))}.
init(_) ->
    Subj = gleam@erlang@process:new_subject(),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        gleam@erlang@process:selecting(
            _pipe,
            Subj,
            fun gleam@function:identity/1
        )
    end,
    {new_state(Subj), {some, Selector}}.

-spec with_func(
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data()))
) -> fun((glisten:message(mist@internal@http2@handler:message()), state(), glisten:connection(mist@internal@http2@handler:message())) -> gleam@otp@actor:next(glisten:message(mist@internal@http2@handler:message()), state())).
with_func(Handler) ->
    fun(Msg, State, Conn) ->
        Sender = erlang:element(4, Conn),
        Conn@1 = {connection,
            {initial, <<>>},
            erlang:element(2, Conn),
            erlang:element(3, Conn)},
        _pipe@12 = case {Msg, State} of
            {{user, {send, _, _}}, {http1, _, _}} ->
                {error,
                    {abnormal,
                        <<"Attempted to send HTTP/2 response without upgrade"/utf8>>}};

            {{user, {send, Id, Resp}}, {http2, State@1}} ->
                _pipe@2 = case erlang:element(4, Resp) of
                    {bytes, Bytes} ->
                        _pipe = Resp,
                        _pipe@1 = gleam@http@response:set_body(_pipe, Bytes),
                        mist@internal@http2:send_bytes_builder(
                            _pipe@1,
                            Conn@1,
                            erlang:element(7, State@1),
                            Id
                        );

                    {file, _, _, _} ->
                        {error,
                            {abnormal,
                                <<"File sending unsupported over HTTP/2"/utf8>>}};

                    {websocket, _} ->
                        {error,
                            {abnormal,
                                <<"WebSocket unsupported for HTTP/2"/utf8>>}};

                    {chunked, _} ->
                        {error,
                            {abnormal,
                                <<"Chunked encoding not supported for HTTP/2"/utf8>>}};

                    {server_sent_events, _} ->
                        {error,
                            {abnormal,
                                <<"Server-Sent Events unsupported for HTTP/2"/utf8>>}}
                end,
                _pipe@3 = gleam@result:map(
                    _pipe@2,
                    fun(Context) ->
                        {http2,
                            mist@internal@http2@handler:send_hpack_context(
                                State@1,
                                Context
                            )}
                    end
                ),
                gleam@result:map_error(
                    _pipe@3,
                    fun(Err) ->
                        logging:log(
                            debug,
                            <<"Error sending HTTP/2 data: "/utf8,
                                (gleam@string:inspect(Err))/binary>>
                        ),
                        Err
                    end
                );

            {{packet, Msg@1}, {http1, State@2, Self}} ->
                _ = case erlang:element(2, State@2) of
                    {some, T} ->
                        gleam@erlang@process:cancel_timer(T);

                    _ ->
                        timer_not_found
                end,
                _pipe@4 = Msg@1,
                _pipe@5 = mist@internal@http:parse_request(_pipe@4, Conn@1),
                _pipe@6 = gleam@result:map_error(
                    _pipe@5,
                    fun(Err@1) -> case Err@1 of
                            discard_packet ->
                                normal;

                            _ ->
                                logging:log(error, gleam@string:inspect(Err@1)),
                                _ = glisten@transport:close(
                                    erlang:element(4, Conn@1),
                                    erlang:element(3, Conn@1)
                                ),
                                {abnormal, <<"Received invalid request"/utf8>>}
                        end end
                ),
                gleam@result:then(_pipe@6, fun(Req) -> case Req of
                            {http1_request, Req@1, Version} ->
                                _pipe@7 = mist@internal@http@handler:call(
                                    Req@1,
                                    Handler,
                                    Conn@1,
                                    Sender,
                                    Version
                                ),
                                gleam@result:map(
                                    _pipe@7,
                                    fun(New_state) ->
                                        {http1, New_state, Self}
                                    end
                                );

                            {upgrade, Data} ->
                                _pipe@8 = mist@internal@http2@handler:upgrade(
                                    Data,
                                    Conn@1,
                                    Self
                                ),
                                gleam@result:map(
                                    _pipe@8,
                                    fun(Field@0) -> {http2, Field@0} end
                                )
                        end end);

            {{packet, Msg@2}, {http2, State@3}} ->
                _pipe@9 = State@3,
                _pipe@10 = mist@internal@http2@handler:append_data(
                    _pipe@9,
                    Msg@2
                ),
                _pipe@11 = mist@internal@http2@handler:call(
                    _pipe@10,
                    Conn@1,
                    Handler
                ),
                gleam@result:map(_pipe@11, fun(Field@0) -> {http2, Field@0} end)
        end,
        _pipe@13 = gleam@result:map(_pipe@12, fun gleam@otp@actor:continue/1),
        _pipe@14 = gleam@result:map_error(
            _pipe@13,
            fun(Field@0) -> {stop, Field@0} end
        ),
        gleam@result:unwrap_both(_pipe@14)
    end.
