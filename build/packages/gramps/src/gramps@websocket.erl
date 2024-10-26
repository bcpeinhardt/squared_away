-module(gramps@websocket).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([frame_to_bytes_builder/2, compressed_frame_to_bytes_builder/3, to_text_frame/3, to_binary_frame/3, aggregate_frames/3, frame_from_message/2, get_messages/3, has_deflate/1, parse_websocket_key/1]).
-export_type([data_frame/0, control_frame/0, frame/0, frame_parse_error/0, parsed_frame/0, sha_hash/0]).

-type data_frame() :: {text_frame, integer(), bitstring()} |
    {binary_frame, integer(), bitstring()}.

-type control_frame() :: {close_frame, integer(), bitstring()} |
    {ping_frame, integer(), bitstring()} |
    {pong_frame, integer(), bitstring()}.

-type frame() :: {data, data_frame()} |
    {control, control_frame()} |
    {continuation, integer(), bitstring()}.

-type frame_parse_error() :: {need_more_data, bitstring()} | invalid_frame.

-type parsed_frame() :: {complete, frame()} | {incomplete, frame()}.

-type sha_hash() :: sha.

-spec mask_data(bitstring(), list(bitstring()), integer(), bitstring()) -> bitstring().
mask_data(Data, Masks, Index, Resp) ->
    case Data of
        <<Masked:8/bitstring, Rest/bitstring>> ->
            [One, Two, Three, Four] = case Masks of
                [_, _, _, _] -> Masks;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"gramps/websocket"/utf8>>,
                                function => <<"mask_data"/utf8>>,
                                line => 38})
            end,
            Mask_value = case Index rem 4 of
                0 ->
                    One;

                1 ->
                    Two;

                2 ->
                    Three;

                3 ->
                    Four;

                _ ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"Somehow a value mod 4 is not 0, 1, 2, or 3"/utf8>>,
                            module => <<"gramps/websocket"/utf8>>,
                            function => <<"mask_data"/utf8>>,
                            line => 44})
            end,
            Unmasked = crypto:exor(Mask_value, Masked),
            mask_data(
                Rest,
                Masks,
                Index + 1,
                <<Resp/bitstring, Unmasked/bitstring>>
            );

        _ ->
            Resp
    end.

-spec make_length(integer()) -> bitstring().
make_length(Length) ->
    case Length of
        Length@1 when Length@1 > 65535 ->
            <<127:7, Length@1:64/integer>>;

        Length@2 when Length@2 >= 126 ->
            <<126:7, Length@2:16/integer>>;

        _ ->
            <<Length:7>>
    end.

-spec make_compressed_frame(
    integer(),
    bitstring(),
    gramps@websocket@compression:context(),
    gleam@option:option(bitstring())
) -> gleam@bytes_builder:bytes_builder().
make_compressed_frame(Opcode, Payload, Context, Mask) ->
    Data = gramps@websocket@compression:deflate(Context, Payload),
    Length = erlang:byte_size(Data),
    Length_section = make_length(Length),
    Masked = case gleam@option:is_some(Mask) of
        true ->
            1;

        false ->
            0
    end,
    Mask_key = gleam@option:unwrap(Mask, <<>>),
    _pipe = <<1:1,
        1:1,
        0:2,
        Opcode:4,
        Masked:1,
        Length_section/bitstring,
        Mask_key/bitstring,
        Data/bitstring>>,
    gleam_stdlib:wrap_list(_pipe).

-spec make_frame(
    integer(),
    integer(),
    bitstring(),
    gleam@option:option(bitstring())
) -> gleam@bytes_builder:bytes_builder().
make_frame(Opcode, Length, Payload, Mask) ->
    Length_section = make_length(Length),
    Masked = case gleam@option:is_some(Mask) of
        true ->
            1;

        false ->
            0
    end,
    Mask_key = gleam@option:unwrap(Mask, <<>>),
    _pipe = <<1:1,
        0:3,
        Opcode:4,
        Masked:1,
        Length_section/bitstring,
        Mask_key/bitstring,
        Payload/bitstring>>,
    gleam_stdlib:wrap_list(_pipe).

-spec frame_to_bytes_builder(frame(), gleam@option:option(bitstring())) -> gleam@bytes_builder:bytes_builder().
frame_to_bytes_builder(Frame, Mask) ->
    case Frame of
        {data, {text_frame, Payload_length, Payload}} ->
            make_frame(1, Payload_length, Payload, Mask);

        {control, {close_frame, Payload_length@1, Payload@1}} ->
            make_frame(8, Payload_length@1, Payload@1, Mask);

        {data, {binary_frame, Payload_length@2, Payload@2}} ->
            make_frame(2, Payload_length@2, Payload@2, Mask);

        {control, {pong_frame, Payload_length@3, Payload@3}} ->
            make_frame(10, Payload_length@3, Payload@3, Mask);

        {control, {ping_frame, Payload_length@4, Payload@4}} ->
            make_frame(9, Payload_length@4, Payload@4, Mask);

        {continuation, Length, Payload@5} ->
            make_frame(0, Length, Payload@5, Mask)
    end.

-spec compressed_frame_to_bytes_builder(
    frame(),
    gramps@websocket@compression:context(),
    gleam@option:option(bitstring())
) -> gleam@bytes_builder:bytes_builder().
compressed_frame_to_bytes_builder(Frame, Context, Mask) ->
    case Frame of
        {data, {text_frame, _, Payload}} ->
            make_compressed_frame(1, Payload, Context, Mask);

        {data, {binary_frame, _, Payload@1}} ->
            make_compressed_frame(2, Payload@1, Context, Mask);

        {control, {close_frame, Payload_length, Payload@2}} ->
            make_frame(8, Payload_length, Payload@2, Mask);

        {control, {pong_frame, Payload_length@1, Payload@3}} ->
            make_frame(10, Payload_length@1, Payload@3, Mask);

        {control, {ping_frame, Payload_length@2, Payload@4}} ->
            make_frame(9, Payload_length@2, Payload@4, Mask);

        {continuation, Length, Payload@5} ->
            make_frame(0, Length, Payload@5, Mask)
    end.

-spec apply_mask(bitstring(), bitstring()) -> bitstring().
apply_mask(Data, Mask) ->
    <<Mask1:1/binary, Mask2:1/binary, Mask3:1/binary, Mask4:1/binary>> = case Mask of
        <<_:1/binary, _:1/binary, _:1/binary, _:1/binary>> -> Mask;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"gramps/websocket"/utf8>>,
                        function => <<"apply_mask"/utf8>>,
                        line => 270})
    end,
    mask_data(Data, [Mask1, Mask2, Mask3, Mask4], 0, <<>>).

-spec to_text_frame(
    binary(),
    gleam@option:option(gramps@websocket@compression:context()),
    gleam@option:option(bitstring())
) -> gleam@bytes_builder:bytes_builder().
to_text_frame(Data, Context, Mask) ->
    Data@1 = gleam_stdlib:identity(Data),
    Data@2 = begin
        _pipe = Mask,
        _pipe@1 = gleam@option:map(
            _pipe,
            fun(_capture) -> apply_mask(Data@1, _capture) end
        ),
        gleam@option:unwrap(_pipe@1, Data@1)
    end,
    Size = erlang:byte_size(Data@2),
    Frame = {data, {text_frame, Size, Data@2}},
    case Context of
        {some, Context@1} ->
            compressed_frame_to_bytes_builder(Frame, Context@1, Mask);

        _ ->
            frame_to_bytes_builder(Frame, Mask)
    end.

-spec to_binary_frame(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context()),
    gleam@option:option(bitstring())
) -> gleam@bytes_builder:bytes_builder().
to_binary_frame(Data, Context, Mask) ->
    Data@1 = begin
        _pipe = Mask,
        _pipe@1 = gleam@option:map(
            _pipe,
            fun(_capture) -> apply_mask(Data, _capture) end
        ),
        gleam@option:unwrap(_pipe@1, Data)
    end,
    Size = erlang:byte_size(Data@1),
    Frame = {data, {binary_frame, Size, Data@1}},
    case Context of
        {some, Context@1} ->
            compressed_frame_to_bytes_builder(Frame, Context@1, Mask);

        _ ->
            frame_to_bytes_builder(Frame, Mask)
    end.

-spec append_frame(frame(), integer(), bitstring()) -> frame().
append_frame(Left, Length, Data) ->
    case Left of
        {data, {text_frame, Len, Payload}} ->
            {data,
                {text_frame,
                    Len + Length,
                    <<Payload/bitstring, Data/bitstring>>}};

        {data, {binary_frame, Len@1, Payload@1}} ->
            {data,
                {binary_frame,
                    Len@1 + Length,
                    <<Payload@1/bitstring, Data/bitstring>>}};

        {control, {close_frame, Len@2, Payload@2}} ->
            {control,
                {close_frame,
                    Len@2 + Length,
                    <<Payload@2/bitstring, Data/bitstring>>}};

        {control, {ping_frame, Len@3, Payload@3}} ->
            {control,
                {ping_frame,
                    Len@3 + Length,
                    <<Payload@3/bitstring, Data/bitstring>>}};

        {control, {pong_frame, Len@4, Payload@4}} ->
            {control,
                {pong_frame,
                    Len@4 + Length,
                    <<Payload@4/bitstring, Data/bitstring>>}};

        {continuation, _, _} ->
            Left
    end.

-spec aggregate_frames(
    list(parsed_frame()),
    gleam@option:option(frame()),
    list(frame())
) -> {ok, list(frame())} | {error, nil}.
aggregate_frames(Frames, Previous, Joined) ->
    case {Frames, Previous} of
        {[], _} ->
            {ok, lists:reverse(Joined)};

        {[{complete, {continuation, Length, Data}} | Rest], {some, Prev}} ->
            Next = append_frame(Prev, Length, Data),
            aggregate_frames(Rest, none, [Next | Joined]);

        {[{incomplete, {continuation, Length@1, Data@1}} | Rest@1],
            {some, Prev@1}} ->
            Next@1 = append_frame(Prev@1, Length@1, Data@1),
            aggregate_frames(Rest@1, {some, Next@1}, Joined);

        {[{incomplete, Frame} | Rest@2], none} ->
            aggregate_frames(Rest@2, {some, Frame}, Joined);

        {[{complete, Frame@1} | Rest@3], none} ->
            aggregate_frames(Rest@3, none, [Frame@1 | Joined]);

        {_, _} ->
            {error, nil}
    end.

-spec inflate(
    boolean(),
    gleam@option:option(gramps@websocket@compression:context()),
    bitstring()
) -> {ok, {integer(), bitstring()}} | {error, frame_parse_error()}.
inflate(Compressed, Context, Data) ->
    case {Compressed, Context} of
        {true, {some, Context@1}} ->
            Data@1 = gramps@websocket@compression:inflate(Context@1, Data),
            Length = erlang:byte_size(Data@1),
            {ok, {Length, Data@1}};

        {true, none} ->
            {error, invalid_frame};

        {_, _} ->
            {ok, {erlang:byte_size(Data), Data}}
    end.

-spec frame_from_message(
    bitstring(),
    gleam@option:option(gramps@websocket@compression:context())
) -> {ok, {parsed_frame(), bitstring()}} | {error, frame_parse_error()}.
frame_from_message(Message, Context) ->
    case Message of
        <<Complete:1,
            Compressed:1,
            _:2,
            Opcode:4/integer,
            Masked:1,
            Payload_length:7/integer,
            Rest/bitstring>> ->
            Compressed@1 = Compressed =:= 1,
            Masked@1 = Masked =:= 1,
            gleam@bool:guard(
                Compressed@1 andalso gleam@option:is_none(Context),
                {error, invalid_frame},
                fun() ->
                    Payload_size = case Payload_length of
                        126 ->
                            16;

                        127 ->
                            64;

                        _ ->
                            0
                    end,
                    Maybe_pair = case {Masked@1, Rest} of
                        {true,
                            <<Length:Payload_size/integer,
                                Mask1:1/binary,
                                Mask2:1/binary,
                                Mask3:1/binary,
                                Mask4:1/binary,
                                Rest@1/bitstring>>} ->
                            Payload_byte_size = case Length of
                                0 ->
                                    Payload_length;

                                N ->
                                    N
                            end,
                            case Rest@1 of
                                <<Payload:Payload_byte_size/binary,
                                    Rest@2/bitstring>> ->
                                    Data = mask_data(
                                        Payload,
                                        [Mask1, Mask2, Mask3, Mask4],
                                        0,
                                        <<>>
                                    ),
                                    {ok, {Data, Rest@2}};

                                _ ->
                                    {error, {need_more_data, Message}}
                            end;

                        {false,
                            <<Length@1:Payload_size/integer, Rest@3/bitstring>>} ->
                            Payload_byte_size@1 = case Length@1 of
                                0 ->
                                    Payload_length;

                                N@1 ->
                                    N@1
                            end,
                            case Rest@3 of
                                <<Payload@1:Payload_byte_size@1/binary,
                                    Rest@4/bitstring>> ->
                                    {ok, {Payload@1, Rest@4}};

                                _ ->
                                    {error, {need_more_data, Message}}
                            end;

                        {_, _} ->
                            {error, invalid_frame}
                    end,
                    gleam@result:'try'(
                        Maybe_pair,
                        fun(_use0) ->
                            {Data@1, Rest@5} = _use0,
                            _pipe@6 = case Opcode of
                                0 ->
                                    _pipe = Data@1,
                                    _pipe@1 = inflate(
                                        Compressed@1,
                                        Context,
                                        _pipe
                                    ),
                                    gleam@result:map(
                                        _pipe@1,
                                        fun(P) ->
                                            {continuation,
                                                erlang:element(1, P),
                                                erlang:element(2, P)}
                                        end
                                    );

                                1 ->
                                    _pipe@2 = Data@1,
                                    _pipe@3 = inflate(
                                        Compressed@1,
                                        Context,
                                        _pipe@2
                                    ),
                                    gleam@result:map(
                                        _pipe@3,
                                        fun(P@1) ->
                                            {data,
                                                {text_frame,
                                                    erlang:element(1, P@1),
                                                    erlang:element(2, P@1)}}
                                        end
                                    );

                                2 ->
                                    _pipe@4 = Data@1,
                                    _pipe@5 = inflate(
                                        Compressed@1,
                                        Context,
                                        _pipe@4
                                    ),
                                    gleam@result:map(
                                        _pipe@5,
                                        fun(P@2) ->
                                            {data,
                                                {binary_frame,
                                                    erlang:element(1, P@2),
                                                    erlang:element(2, P@2)}}
                                        end
                                    );

                                8 ->
                                    {ok,
                                        {control,
                                            {close_frame,
                                                Payload_length,
                                                Data@1}}};

                                9 ->
                                    {ok,
                                        {control,
                                            {ping_frame, Payload_length, Data@1}}};

                                10 ->
                                    {ok,
                                        {control,
                                            {pong_frame, Payload_length, Data@1}}};

                                _ ->
                                    {error, invalid_frame}
                            end,
                            gleam@result:then(
                                _pipe@6,
                                fun(Frame) -> case Complete of
                                        1 ->
                                            {ok, {{complete, Frame}, Rest@5}};

                                        0 ->
                                            {ok, {{incomplete, Frame}, Rest@5}};

                                        _ ->
                                            {error, invalid_frame}
                                    end end
                            )
                        end
                    )
                end
            );

        _ ->
            {error, invalid_frame}
    end.

-spec get_messages(
    bitstring(),
    list(parsed_frame()),
    gleam@option:option(gramps@websocket@compression:context())
) -> {list(parsed_frame()), bitstring()}.
get_messages(Data, Frames, Context) ->
    case frame_from_message(Data, Context) of
        {ok, {Frame, <<>>}} ->
            {lists:reverse([Frame | Frames]), <<>>};

        {ok, {Frame@1, Rest}} ->
            get_messages(Rest, [Frame@1 | Frames], Context);

        {error, {need_more_data, Rest@1}} ->
            {lists:reverse(Frames), Rest@1};

        {error, invalid_frame} ->
            {lists:reverse(Frames), Data}
    end.

-spec has_deflate(list(binary())) -> boolean().
has_deflate(Extensions) ->
    gleam@list:any(
        Extensions,
        fun(Str) -> Str =:= <<"permessage-deflate"/utf8>> end
    ).

-spec parse_websocket_key(binary()) -> binary().
parse_websocket_key(Key) ->
    _pipe = Key,
    _pipe@1 = gleam@string:append(
        _pipe,
        <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"/utf8>>
    ),
    _pipe@2 = crypto:hash(sha, _pipe@1),
    base64:encode(_pipe@2).
