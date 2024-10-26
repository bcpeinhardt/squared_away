-module(wisp@testing).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([string_body/1, bit_array_body/1, request/4, get/2, post/3, post_form/3, post_json/3, head/2, put/3, put_form/3, put_json/3, delete/3, delete_form/3, delete_json/3, trace/2, connect/2, options/2, patch/3, patch_form/3, patch_json/3, set_cookie/4]).

-spec string_body(gleam@http@response:response(wisp:body())) -> binary().
string_body(Response) ->
    case erlang:element(4, Response) of
        empty ->
            <<""/utf8>>;

        {text, Builder} ->
            gleam@string_builder:to_string(Builder);

        {bytes, Bytes} ->
            Data = erlang:list_to_bitstring(Bytes),
            _assert_subject = gleam@bit_array:to_string(Data),
            {ok, String} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"wisp/testing"/utf8>>,
                                function => <<"string_body"/utf8>>,
                                line => 233})
            end,
            String;

        {file, Path} ->
            _assert_subject@1 = simplifile:read(Path),
            {ok, Contents} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"wisp/testing"/utf8>>,
                                function => <<"string_body"/utf8>>,
                                line => 237})
            end,
            Contents
    end.

-spec bit_array_body(gleam@http@response:response(wisp:body())) -> bitstring().
bit_array_body(Response) ->
    case erlang:element(4, Response) of
        empty ->
            <<>>;

        {bytes, Builder} ->
            erlang:list_to_bitstring(Builder);

        {text, Builder@1} ->
            erlang:list_to_bitstring(gleam_stdlib:wrap_list(Builder@1));

        {file, Path} ->
            _assert_subject = simplifile_erl:read_bits(Path),
            {ok, Contents} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"wisp/testing"/utf8>>,
                                function => <<"bit_array_body"/utf8>>,
                                line => 257})
            end,
            Contents
    end.

-spec request(
    gleam@http:method(),
    binary(),
    list({binary(), binary()}),
    bitstring()
) -> gleam@http@request:request(wisp@internal:connection()).
request(Method, Path, Headers, Body) ->
    {Path@2, Query@1} = case gleam@string:split(Path, <<"?"/utf8>>) of
        [Path@1, Query] ->
            {Path@1, {some, Query}};

        _ ->
            {Path, none}
    end,
    _pipe = {request,
        Method,
        Headers,
        Body,
        https,
        <<"localhost"/utf8>>,
        none,
        Path@2,
        Query@1},
    gleam@http@request:set_body(
        _pipe,
        wisp:create_canned_connection(
            Body,
            <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"/utf8>>
        )
    ).

-spec get(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
get(Path, Headers) ->
    request(get, Path, Headers, <<>>).

-spec post(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
post(Path, Headers, Body) ->
    request(post, Path, Headers, <<Body/binary>>).

-spec post_form(
    binary(),
    list({binary(), binary()}),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
post_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(post, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-spec post_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
post_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(post, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-spec head(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
head(Path, Headers) ->
    request(head, Path, Headers, <<>>).

-spec put(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
put(Path, Headers, Body) ->
    request(put, Path, Headers, <<Body/binary>>).

-spec put_form(binary(), list({binary(), binary()}), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
put_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(put, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-spec put_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
put_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(put, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-spec delete(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
delete(Path, Headers, Body) ->
    request(delete, Path, Headers, <<Body/binary>>).

-spec delete_form(
    binary(),
    list({binary(), binary()}),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
delete_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(delete, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-spec delete_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
delete_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(delete, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-spec trace(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
trace(Path, Headers) ->
    request(trace, Path, Headers, <<>>).

-spec connect(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
connect(Path, Headers) ->
    request(connect, Path, Headers, <<>>).

-spec options(binary(), list({binary(), binary()})) -> gleam@http@request:request(wisp@internal:connection()).
options(Path, Headers) ->
    request(options, Path, Headers, <<>>).

-spec patch(binary(), list({binary(), binary()}), binary()) -> gleam@http@request:request(wisp@internal:connection()).
patch(Path, Headers, Body) ->
    request(patch, Path, Headers, <<Body/binary>>).

-spec patch_form(
    binary(),
    list({binary(), binary()}),
    list({binary(), binary()})
) -> gleam@http@request:request(wisp@internal:connection()).
patch_form(Path, Headers, Data) ->
    Body = gleam@uri:query_to_string(Data),
    _pipe = request(patch, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/x-www-form-urlencoded"/utf8>>
    ).

-spec patch_json(binary(), list({binary(), binary()}), gleam@json:json()) -> gleam@http@request:request(wisp@internal:connection()).
patch_json(Path, Headers, Data) ->
    Body = gleam@json:to_string(Data),
    _pipe = request(patch, Path, Headers, <<Body/binary>>),
    gleam@http@request:set_header(
        _pipe,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-spec set_cookie(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    wisp:security()
) -> gleam@http@request:request(wisp@internal:connection()).
set_cookie(Req, Name, Value, Security) ->
    Value@1 = case Security of
        plain_text ->
            gleam_stdlib:bit_array_base64_encode(<<Value/binary>>, false);

        signed ->
            wisp:sign_message(Req, <<Value/binary>>, sha512)
    end,
    gleam@http@request:set_cookie(Req, Name, Value@1).
