-module(mist@internal@http2).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([default_settings/0, update_settings/2, send_frame/3, hpack_new_context/1, hpack_max_table_size/2, hpack_decode/2, hpack_encode/2, send_bytes_builder/4]).
-export_type([http2_settings/0, hpack_context/0, hpack_error/0]).

-type http2_settings() :: {http2_settings,
        integer(),
        mist@internal@http2@frame:push_state(),
        integer(),
        integer(),
        integer(),
        gleam@option:option(integer())}.

-type hpack_context() :: any().

-type hpack_error() :: compression | {bad_header_packet, bitstring()}.

-spec default_settings() -> http2_settings().
default_settings() ->
    {http2_settings, 4096, disabled, 100, 65535, 16384, none}.

-spec update_settings(
    http2_settings(),
    list(mist@internal@http2@frame:setting())
) -> http2_settings().
update_settings(Current, Settings) ->
    gleam@list:fold(
        Settings,
        Current,
        fun(Settings@1, Setting) -> case Setting of
                {header_table_size, Size} ->
                    erlang:setelement(2, Settings@1, Size);

                {server_push, Push} ->
                    erlang:setelement(3, Settings@1, Push);

                {max_concurrent_streams, Max} ->
                    erlang:setelement(4, Settings@1, Max);

                {initial_window_size, Size@1} ->
                    erlang:setelement(5, Settings@1, Size@1);

                {max_frame_size, Size@2} ->
                    erlang:setelement(6, Settings@1, Size@2);

                {max_header_list_size, Size@3} ->
                    erlang:setelement(7, Settings@1, {some, Size@3})
            end end
    ).

-spec send_data(
    mist@internal@http:connection(),
    bitstring(),
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame()),
    boolean()
) -> {ok, nil} | {error, gleam@erlang@process:exit_reason()}.
send_data(Conn, Data, Stream_identifier, End_stream) ->
    Data_frame = {data, Data, End_stream, Stream_identifier},
    Encoded = mist@internal@http2@frame:encode(Data_frame),
    _pipe = glisten@transport:send(
        erlang:element(4, Conn),
        erlang:element(3, Conn),
        gleam_stdlib:wrap_list(Encoded)
    ),
    gleam@result:map_error(
        _pipe,
        fun(Err) ->
            logging:log(
                debug,
                <<"failed to send :(  "/utf8,
                    (gleam@erlang:format(Err))/binary>>
            ),
            {abnormal, <<"Failed to send HTTP/2 data"/utf8>>}
        end
    ).

-spec send_frame(
    mist@internal@http2@frame:frame(),
    glisten@socket:socket(),
    glisten@transport:transport()
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
send_frame(Frame_to_send, Socket, Transport) ->
    Data = mist@internal@http2@frame:encode(Frame_to_send),
    glisten@transport:send(Transport, Socket, gleam_stdlib:wrap_list(Data)).

-spec hpack_new_context(integer()) -> hpack_context().
hpack_new_context(Size) ->
    hpack:new_context(Size).

-spec hpack_max_table_size(hpack_context(), integer()) -> hpack_context().
hpack_max_table_size(Context, Size) ->
    mist_ffi:hpack_new_max_table_size(Context, Size).

-spec hpack_decode(hpack_context(), bitstring()) -> {ok,
        {list({binary(), binary()}), hpack_context()}} |
    {error, hpack_error()}.
hpack_decode(Context, Bin) ->
    mist_ffi:hpack_decode(Context, Bin).

-spec hpack_encode(hpack_context(), list({binary(), binary()})) -> {ok,
        {bitstring(), hpack_context()}} |
    {error, any()}.
hpack_encode(Context, Headers) ->
    mist_ffi:hpack_encode(Context, Headers).

-spec send_headers(
    hpack_context(),
    mist@internal@http:connection(),
    list({binary(), binary()}),
    boolean(),
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
) -> {ok, hpack_context()} | {error, gleam@erlang@process:exit_reason()}.
send_headers(Context, Conn, Headers, End_stream, Stream_identifier) ->
    _pipe = mist_ffi:hpack_encode(Context, Headers),
    gleam@result:then(
        _pipe,
        fun(Pair) ->
            {Headers@1, New_context} = Pair,
            Header_frame = {header,
                {complete, Headers@1},
                End_stream,
                Stream_identifier,
                none},
            Encoded = mist@internal@http2@frame:encode(Header_frame),
            case glisten@transport:send(
                erlang:element(4, Conn),
                erlang:element(3, Conn),
                gleam_stdlib:wrap_list(Encoded)
            ) of
                {ok, _} ->
                    {ok, New_context};

                {error, _} ->
                    {error,
                        {abnormal, <<"Failed to send HTTP/2 headers"/utf8>>}}
            end
        end
    ).

-spec send_bytes_builder(
    gleam@http@response:response(gleam@bytes_builder:bytes_builder()),
    mist@internal@http:connection(),
    hpack_context(),
    mist@internal@http2@frame:stream_identifier(mist@internal@http2@frame:frame())
) -> {ok, hpack_context()} | {error, gleam@erlang@process:exit_reason()}.
send_bytes_builder(Resp, Conn, Context, Id) ->
    Resp@1 = begin
        _pipe = Resp,
        mist@internal@http:add_default_headers(_pipe, false)
    end,
    Headers = [{<<":status"/utf8>>,
            gleam@int:to_string(erlang:element(2, Resp@1))} |
        erlang:element(3, Resp@1)],
    case erlang:iolist_size(erlang:element(4, Resp@1)) of
        0 ->
            send_headers(Context, Conn, Headers, true, Id);

        _ ->
            _pipe@1 = send_headers(Context, Conn, Headers, false, Id),
            gleam@result:then(
                _pipe@1,
                fun(Context@1) ->
                    _pipe@2 = send_data(
                        Conn,
                        erlang:list_to_bitstring(erlang:element(4, Resp@1)),
                        Id,
                        true
                    ),
                    gleam@result:replace(_pipe@2, Context@1)
                end
            )
    end.
