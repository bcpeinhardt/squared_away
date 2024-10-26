-module(wisp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([response/1, set_body/2, file_download/3, file_download_from_memory/3, html_response/2, json_response/2, html_body/2, json_body/2, string_builder_body/2, string_body/2, method_not_allowed/1, ok/0, created/0, accepted/0, redirect/1, moved_permanently/1, no_content/0, not_found/0, bad_request/0, entity_too_large/0, unsupported_media_type/1, unprocessable_entity/0, internal_server_error/0, set_max_body_size/2, get_max_body_size/1, set_secret_key_base/2, get_secret_key_base/1, set_max_files_size/2, get_max_files_size/1, set_read_chunk_size/2, get_read_chunk_size/1, require_method/3, get_query/1, method_override/1, read_body_to_bitstring/1, require_bit_array_body/2, require_content_type/3, require_string_body/2, require_json/2, serve_static/4, handle_head/2, new_temporary_file/1, delete_temporary_files/1, configure_logger/0, set_logger_level/1, log_emergency/1, log_alert/1, log_critical/1, log_error/1, require_form/2, rescue_crashes/1, log_warning/1, log_notice/1, log_info/1, log_request/2, log_debug/1, random_string/1, sign_message/3, verify_signed_message/2, set_cookie/6, get_cookie/3, create_canned_connection/2, escape_html/1]).
-export_type([body/0, buffered_reader/0, quotas/0, form_data/0, uploaded_file/0, do_not_leak/0, error_kind/0, log_level/0, security/0]).

-type body() :: {text, gleam@string_builder:string_builder()} |
    {bytes, gleam@bytes_builder:bytes_builder()} |
    {file, binary()} |
    empty.

-type buffered_reader() :: {buffered_reader,
        fun((integer()) -> {ok, wisp@internal:read()} | {error, nil}),
        bitstring()}.

-type quotas() :: {quotas, integer(), integer()}.

-type form_data() :: {form_data,
        list({binary(), binary()}),
        list({binary(), uploaded_file()})}.

-type uploaded_file() :: {uploaded_file, binary(), binary()}.

-type do_not_leak() :: any().

-type error_kind() :: errored | thrown | exited.

-type log_level() :: emergency_level |
    alert_level |
    critical_level |
    error_level |
    warning_level |
    notice_level |
    info_level |
    debug_level.

-type security() :: plain_text | signed.

-spec response(integer()) -> gleam@http@response:response(body()).
response(Status) ->
    {response, Status, [], empty}.

-spec set_body(gleam@http@response:response(body()), body()) -> gleam@http@response:response(body()).
set_body(Response, Body) ->
    _pipe = Response,
    gleam@http@response:set_body(_pipe, Body).

-spec file_download(gleam@http@response:response(body()), binary(), binary()) -> gleam@http@response:response(body()).
file_download(Response, Name, Path) ->
    Name@1 = gleam@uri:percent_encode(Name),
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_header(
        _pipe,
        <<"content-disposition"/utf8>>,
        <<<<"attachment; filename=\""/utf8, Name@1/binary>>/binary, "\""/utf8>>
    ),
    gleam@http@response:set_body(_pipe@1, {file, Path}).

-spec file_download_from_memory(
    gleam@http@response:response(body()),
    binary(),
    gleam@bytes_builder:bytes_builder()
) -> gleam@http@response:response(body()).
file_download_from_memory(Response, Name, Data) ->
    Name@1 = gleam@uri:percent_encode(Name),
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_header(
        _pipe,
        <<"content-disposition"/utf8>>,
        <<<<"attachment; filename=\""/utf8, Name@1/binary>>/binary, "\""/utf8>>
    ),
    gleam@http@response:set_body(_pipe@1, {bytes, Data}).

-spec html_response(gleam@string_builder:string_builder(), integer()) -> gleam@http@response:response(body()).
html_response(Html, Status) ->
    {response,
        Status,
        [{<<"content-type"/utf8>>, <<"text/html; charset=utf-8"/utf8>>}],
        {text, Html}}.

-spec json_response(gleam@string_builder:string_builder(), integer()) -> gleam@http@response:response(body()).
json_response(Json, Status) ->
    {response,
        Status,
        [{<<"content-type"/utf8>>, <<"application/json; charset=utf-8"/utf8>>}],
        {text, Json}}.

-spec html_body(
    gleam@http@response:response(body()),
    gleam@string_builder:string_builder()
) -> gleam@http@response:response(body()).
html_body(Response, Html) ->
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_body(_pipe, {text, Html}),
    gleam@http@response:set_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        <<"text/html; charset=utf-8"/utf8>>
    ).

-spec json_body(
    gleam@http@response:response(body()),
    gleam@string_builder:string_builder()
) -> gleam@http@response:response(body()).
json_body(Response, Json) ->
    _pipe = Response,
    _pipe@1 = gleam@http@response:set_body(_pipe, {text, Json}),
    gleam@http@response:set_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        <<"application/json; charset=utf-8"/utf8>>
    ).

-spec string_builder_body(
    gleam@http@response:response(body()),
    gleam@string_builder:string_builder()
) -> gleam@http@response:response(body()).
string_builder_body(Response, Content) ->
    _pipe = Response,
    gleam@http@response:set_body(_pipe, {text, Content}).

-spec string_body(gleam@http@response:response(body()), binary()) -> gleam@http@response:response(body()).
string_body(Response, Content) ->
    _pipe = Response,
    gleam@http@response:set_body(
        _pipe,
        {text, gleam@string_builder:from_string(Content)}
    ).

-spec method_not_allowed(list(gleam@http:method())) -> gleam@http@response:response(body()).
method_not_allowed(Methods) ->
    Allowed = begin
        _pipe = Methods,
        _pipe@1 = gleam@list:map(_pipe, fun gleam@http:method_to_string/1),
        _pipe@2 = gleam@list:sort(_pipe@1, fun gleam@string:compare/2),
        _pipe@3 = gleam@string:join(_pipe@2, <<", "/utf8>>),
        gleam@string:uppercase(_pipe@3)
    end,
    {response, 405, [{<<"allow"/utf8>>, Allowed}], empty}.

-spec ok() -> gleam@http@response:response(body()).
ok() ->
    {response, 200, [], empty}.

-spec created() -> gleam@http@response:response(body()).
created() ->
    {response, 201, [], empty}.

-spec accepted() -> gleam@http@response:response(body()).
accepted() ->
    {response, 202, [], empty}.

-spec redirect(binary()) -> gleam@http@response:response(body()).
redirect(Url) ->
    {response, 303, [{<<"location"/utf8>>, Url}], empty}.

-spec moved_permanently(binary()) -> gleam@http@response:response(body()).
moved_permanently(Url) ->
    {response, 308, [{<<"location"/utf8>>, Url}], empty}.

-spec no_content() -> gleam@http@response:response(body()).
no_content() ->
    {response, 204, [], empty}.

-spec not_found() -> gleam@http@response:response(body()).
not_found() ->
    {response, 404, [], empty}.

-spec bad_request() -> gleam@http@response:response(body()).
bad_request() ->
    {response, 400, [], empty}.

-spec entity_too_large() -> gleam@http@response:response(body()).
entity_too_large() ->
    {response, 413, [], empty}.

-spec unsupported_media_type(list(binary())) -> gleam@http@response:response(body()).
unsupported_media_type(Acceptable) ->
    Acceptable@1 = gleam@string:join(Acceptable, <<", "/utf8>>),
    {response, 415, [{<<"accept"/utf8>>, Acceptable@1}], empty}.

-spec unprocessable_entity() -> gleam@http@response:response(body()).
unprocessable_entity() ->
    {response, 422, [], empty}.

-spec internal_server_error() -> gleam@http@response:response(body()).
internal_server_error() ->
    {response, 500, [], empty}.

-spec decrement_body_quota(quotas(), integer()) -> {ok, quotas()} |
    {error, gleam@http@response:response(body())}.
decrement_body_quota(Quotas, Size) ->
    Quotas@1 = erlang:setelement(2, Quotas, erlang:element(2, Quotas) - Size),
    case erlang:element(2, Quotas@1) < 0 of
        true ->
            {error, entity_too_large()};

        false ->
            {ok, Quotas@1}
    end.

-spec decrement_quota(integer(), integer()) -> {ok, integer()} |
    {error, gleam@http@response:response(body())}.
decrement_quota(Quota, Size) ->
    case Quota - Size of
        Quota@1 when Quota@1 < 0 ->
            {error, entity_too_large()};

        Quota@2 ->
            {ok, Quota@2}
    end.

-spec buffered_read(buffered_reader(), integer()) -> {ok, wisp@internal:read()} |
    {error, nil}.
buffered_read(Reader, Chunk_size) ->
    case erlang:element(3, Reader) of
        <<>> ->
            (erlang:element(2, Reader))(Chunk_size);

        _ ->
            {ok, {chunk, erlang:element(3, Reader), erlang:element(2, Reader)}}
    end.

-spec set_max_body_size(
    gleam@http@request:request(wisp@internal:connection()),
    integer()
) -> gleam@http@request:request(wisp@internal:connection()).
set_max_body_size(Request, Size) ->
    _pipe = erlang:setelement(3, erlang:element(4, Request), Size),
    gleam@http@request:set_body(Request, _pipe).

-spec get_max_body_size(gleam@http@request:request(wisp@internal:connection())) -> integer().
get_max_body_size(Request) ->
    erlang:element(3, erlang:element(4, Request)).

-spec set_secret_key_base(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> gleam@http@request:request(wisp@internal:connection()).
set_secret_key_base(Request, Key) ->
    case erlang:byte_size(Key) < 64 of
        true ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Secret key base must be at least 64 bytes long"/utf8>>,
                    module => <<"wisp"/utf8>>,
                    function => <<"set_secret_key_base"/utf8>>,
                    line => 705});

        false ->
            _pipe = erlang:setelement(6, erlang:element(4, Request), Key),
            gleam@http@request:set_body(Request, _pipe)
    end.

-spec get_secret_key_base(
    gleam@http@request:request(wisp@internal:connection())
) -> binary().
get_secret_key_base(Request) ->
    erlang:element(6, erlang:element(4, Request)).

-spec set_max_files_size(
    gleam@http@request:request(wisp@internal:connection()),
    integer()
) -> gleam@http@request:request(wisp@internal:connection()).
set_max_files_size(Request, Size) ->
    _pipe = erlang:setelement(4, erlang:element(4, Request), Size),
    gleam@http@request:set_body(Request, _pipe).

-spec get_max_files_size(gleam@http@request:request(wisp@internal:connection())) -> integer().
get_max_files_size(Request) ->
    erlang:element(4, erlang:element(4, Request)).

-spec set_read_chunk_size(
    gleam@http@request:request(wisp@internal:connection()),
    integer()
) -> gleam@http@request:request(wisp@internal:connection()).
set_read_chunk_size(Request, Size) ->
    _pipe = erlang:setelement(5, erlang:element(4, Request), Size),
    gleam@http@request:set_body(Request, _pipe).

-spec get_read_chunk_size(
    gleam@http@request:request(wisp@internal:connection())
) -> integer().
get_read_chunk_size(Request) ->
    erlang:element(5, erlang:element(4, Request)).

-spec require_method(
    gleam@http@request:request(any()),
    gleam@http:method(),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_method(Request, Method, Next) ->
    case erlang:element(2, Request) =:= Method of
        true ->
            Next();

        false ->
            method_not_allowed([Method])
    end.

-spec get_query(gleam@http@request:request(wisp@internal:connection())) -> list({binary(),
    binary()}).
get_query(Request) ->
    _pipe = gleam@http@request:get_query(Request),
    gleam@result:unwrap(_pipe, []).

-spec method_override(gleam@http@request:request(ACNG)) -> gleam@http@request:request(ACNG).
method_override(Request) ->
    gleam@bool:guard(
        erlang:element(2, Request) /= post,
        Request,
        fun() ->
            _pipe = (gleam@result:'try'(
                gleam@http@request:get_query(Request),
                fun(Query) ->
                    gleam@result:'try'(
                        gleam@list:key_find(Query, <<"_method"/utf8>>),
                        fun(Value) ->
                            gleam@result:map(
                                gleam@http:parse_method(Value),
                                fun(Method) -> case Method of
                                        put ->
                                            gleam@http@request:set_method(
                                                Request,
                                                Method
                                            );

                                        patch ->
                                            gleam@http@request:set_method(
                                                Request,
                                                Method
                                            );

                                        delete ->
                                            gleam@http@request:set_method(
                                                Request,
                                                Method
                                            );

                                        _ ->
                                            Request
                                    end end
                            )
                        end
                    )
                end
            )),
            gleam@result:unwrap(_pipe, Request)
        end
    ).

-spec read_body_loop(
    fun((integer()) -> {ok, wisp@internal:read()} | {error, nil}),
    integer(),
    integer(),
    bitstring()
) -> {ok, bitstring()} | {error, nil}.
read_body_loop(Reader, Read_chunk_size, Max_body_size, Accumulator) ->
    gleam@result:'try'(Reader(Read_chunk_size), fun(Chunk) -> case Chunk of
                reading_finished ->
                    {ok, Accumulator};

                {chunk, Chunk@1, Next} ->
                    Accumulator@1 = gleam@bit_array:append(Accumulator, Chunk@1),
                    case erlang:byte_size(Accumulator@1) > Max_body_size of
                        true ->
                            {error, nil};

                        false ->
                            read_body_loop(
                                Next,
                                Read_chunk_size,
                                Max_body_size,
                                Accumulator@1
                            )
                    end
            end end).

-spec read_body_to_bitstring(
    gleam@http@request:request(wisp@internal:connection())
) -> {ok, bitstring()} | {error, nil}.
read_body_to_bitstring(Request) ->
    Connection = erlang:element(4, Request),
    read_body_loop(
        erlang:element(2, Connection),
        erlang:element(5, Connection),
        erlang:element(3, Connection),
        <<>>
    ).

-spec require_bit_array_body(
    gleam@http@request:request(wisp@internal:connection()),
    fun((bitstring()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_bit_array_body(Request, Next) ->
    case read_body_to_bitstring(Request) of
        {ok, Body} ->
            Next(Body);

        {error, _} ->
            entity_too_large()
    end.

-spec require_content_type(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_content_type(Request, Expected, Next) ->
    case gleam@list:key_find(
        erlang:element(3, Request),
        <<"content-type"/utf8>>
    ) of
        {ok, Content_type} ->
            case gleam@string:split_once(Content_type, <<";"/utf8>>) of
                {ok, {Content_type@1, _}} when Content_type@1 =:= Expected ->
                    Next();

                _ when Content_type =:= Expected ->
                    Next();

                _ ->
                    unsupported_media_type([Expected])
            end;

        _ ->
            unsupported_media_type([Expected])
    end.

-spec bit_array_to_string(bitstring()) -> {ok, binary()} |
    {error, gleam@http@response:response(body())}.
bit_array_to_string(Bits) ->
    _pipe = gleam@bit_array:to_string(Bits),
    gleam@result:replace_error(_pipe, bad_request()).

-spec fn_with_bad_request_error(fun((ACOH) -> {ok, ACOI} | {error, any()})) -> fun((ACOH) -> {ok,
        ACOI} |
    {error, gleam@http@response:response(body())}).
fn_with_bad_request_error(F) ->
    fun(A) -> _pipe = F(A),
        gleam@result:replace_error(_pipe, bad_request()) end.

-spec multipart_content_disposition(list({binary(), binary()})) -> {ok,
        {binary(), gleam@option:option(binary())}} |
    {error, gleam@http@response:response(body())}.
multipart_content_disposition(Headers) ->
    _pipe = (gleam@result:'try'(
        gleam@list:key_find(Headers, <<"content-disposition"/utf8>>),
        fun(Header) ->
            gleam@result:'try'(
                gleam@http:parse_content_disposition(Header),
                fun(Header@1) ->
                    gleam@result:map(
                        gleam@list:key_find(
                            erlang:element(3, Header@1),
                            <<"name"/utf8>>
                        ),
                        fun(Name) ->
                            Filename = gleam@option:from_result(
                                gleam@list:key_find(
                                    erlang:element(3, Header@1),
                                    <<"filename"/utf8>>
                                )
                            ),
                            {Name, Filename}
                        end
                    )
                end
            )
        end
    )),
    gleam@result:replace_error(_pipe, bad_request()).

-spec read_chunk(buffered_reader(), integer()) -> {ok,
        {bitstring(),
            fun((integer()) -> {ok, wisp@internal:read()} | {error, nil})}} |
    {error, gleam@http@response:response(body())}.
read_chunk(Reader, Chunk_size) ->
    _pipe = buffered_read(Reader, Chunk_size),
    _pipe@1 = gleam@result:replace_error(_pipe, bad_request()),
    gleam@result:'try'(_pipe@1, fun(Chunk) -> case Chunk of
                {chunk, Chunk@1, Next} ->
                    {ok, {Chunk@1, Next}};

                reading_finished ->
                    {error, bad_request()}
            end end).

-spec multipart_body(
    buffered_reader(),
    fun((bitstring()) -> {ok, gleam@http:multipart_body()} |
        {error, gleam@http@response:response(body())}),
    binary(),
    integer(),
    integer(),
    fun((ACOB, bitstring()) -> {ok, ACOB} |
        {error, gleam@http@response:response(body())}),
    ACOB
) -> {ok, {gleam@option:option(buffered_reader()), integer(), ACOB}} |
    {error, gleam@http@response:response(body())}.
multipart_body(Reader, Parse, Boundary, Chunk_size, Quota, Append, Data) ->
    gleam@result:'try'(
        read_chunk(Reader, Chunk_size),
        fun(_use0) ->
            {Chunk, Reader@1} = _use0,
            Size_read = erlang:byte_size(Chunk),
            gleam@result:'try'(Parse(Chunk), fun(Output) -> case Output of
                        {multipart_body, Parsed, Done, Remaining} ->
                            Used = (Size_read - erlang:byte_size(Remaining)) - 2,
                            Used@1 = case Done of
                                true ->
                                    (Used - 4) - erlang:byte_size(Boundary);

                                false ->
                                    Used
                            end,
                            gleam@result:'try'(
                                decrement_quota(Quota, Used@1),
                                fun(Quota@1) ->
                                    Reader@2 = {buffered_reader,
                                        Reader@1,
                                        Remaining},
                                    Reader@3 = case Done of
                                        true ->
                                            none;

                                        false ->
                                            {some, Reader@2}
                                    end,
                                    gleam@result:map(
                                        Append(Data, Parsed),
                                        fun(Value) ->
                                            {Reader@3, Quota@1, Value}
                                        end
                                    )
                                end
                            );

                        {more_required_for_body, Chunk@1, Parse@1} ->
                            Parse@2 = fn_with_bad_request_error(
                                fun(_capture) -> Parse@1(_capture) end
                            ),
                            Reader@4 = {buffered_reader, Reader@1, <<>>},
                            gleam@result:'try'(
                                Append(Data, Chunk@1),
                                fun(Data@1) ->
                                    multipart_body(
                                        Reader@4,
                                        Parse@2,
                                        Boundary,
                                        Chunk_size,
                                        Quota,
                                        Append,
                                        Data@1
                                    )
                                end
                            )
                    end end)
        end
    ).

-spec multipart_headers(
    buffered_reader(),
    fun((bitstring()) -> {ok, gleam@http:multipart_headers()} |
        {error, gleam@http@response:response(body())}),
    integer(),
    quotas()
) -> {ok, {list({binary(), binary()}), buffered_reader(), quotas()}} |
    {error, gleam@http@response:response(body())}.
multipart_headers(Reader, Parse, Chunk_size, Quotas) ->
    gleam@result:'try'(
        read_chunk(Reader, Chunk_size),
        fun(_use0) ->
            {Chunk, Reader@1} = _use0,
            gleam@result:'try'(Parse(Chunk), fun(Headers) -> case Headers of
                        {multipart_headers, Headers@1, Remaining} ->
                            Used = erlang:byte_size(Chunk) - erlang:byte_size(
                                Remaining
                            ),
                            gleam@result:map(
                                decrement_body_quota(Quotas, Used),
                                fun(Quotas@1) ->
                                    Reader@2 = {buffered_reader,
                                        Reader@1,
                                        Remaining},
                                    {Headers@1, Reader@2, Quotas@1}
                                end
                            );

                        {more_required_for_headers, Parse@1} ->
                            Parse@2 = fun(Chunk@1) -> _pipe = Parse@1(Chunk@1),
                                gleam@result:replace_error(_pipe, bad_request()) end,
                            Reader@3 = {buffered_reader, Reader@1, <<>>},
                            multipart_headers(
                                Reader@3,
                                Parse@2,
                                Chunk_size,
                                Quotas
                            )
                    end end)
        end
    ).

-spec sort_keys(list({binary(), ACOZ})) -> list({binary(), ACOZ}).
sort_keys(Pairs) ->
    gleam@list:sort(
        Pairs,
        fun(A, B) ->
            gleam@string:compare(erlang:element(1, A), erlang:element(1, B))
        end
    ).

-spec or_400(
    {ok, ACPC} | {error, any()},
    fun((ACPC) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
or_400(Result, Next) ->
    case Result of
        {ok, Value} ->
            Next(Value);

        {error, _} ->
            bad_request()
    end.

-spec require_string_body(
    gleam@http@request:request(wisp@internal:connection()),
    fun((binary()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_string_body(Request, Next) ->
    case read_body_to_bitstring(Request) of
        {ok, Body} ->
            or_400(gleam@bit_array:to_string(Body), Next);

        {error, _} ->
            entity_too_large()
    end.

-spec require_json(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@dynamic:dynamic_()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_json(Request, Next) ->
    require_content_type(
        Request,
        <<"application/json"/utf8>>,
        fun() ->
            require_string_body(
                Request,
                fun(Body) ->
                    or_400(
                        gleam@json:decode(
                            Body,
                            fun(Field@0) -> {ok, Field@0} end
                        ),
                        fun(Json) -> Next(Json) end
                    )
                end
            )
        end
    ).

-spec require_urlencoded_form(
    gleam@http@request:request(wisp@internal:connection()),
    fun((form_data()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_urlencoded_form(Request, Next) ->
    require_string_body(
        Request,
        fun(Body) ->
            or_400(
                gleam@uri:parse_query(Body),
                fun(Pairs) ->
                    Pairs@1 = sort_keys(Pairs),
                    Next({form_data, Pairs@1, []})
                end
            )
        end
    ).

-spec serve_static(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
serve_static(Req, Prefix, Directory, Handler) ->
    Path = wisp@internal:remove_preceeding_slashes(erlang:element(8, Req)),
    Prefix@1 = wisp@internal:remove_preceeding_slashes(Prefix),
    case {erlang:element(2, Req), gleam@string:starts_with(Path, Prefix@1)} of
        {get, true} ->
            Path@1 = begin
                _pipe = Path,
                _pipe@1 = gleam@string:drop_left(
                    _pipe,
                    gleam@string:length(Prefix@1)
                ),
                _pipe@2 = gleam@string:replace(
                    _pipe@1,
                    <<".."/utf8>>,
                    <<""/utf8>>
                ),
                wisp@internal:join_path(Directory, _pipe@2)
            end,
            Mime_type = begin
                _pipe@3 = erlang:element(8, Req),
                _pipe@4 = gleam@string:split(_pipe@3, <<"."/utf8>>),
                _pipe@5 = gleam@list:last(_pipe@4),
                _pipe@6 = gleam@result:unwrap(_pipe@5, <<""/utf8>>),
                marceau:extension_to_mime_type(_pipe@6)
            end,
            Content_type = case Mime_type of
                <<"application/json"/utf8>> ->
                    <<Mime_type/binary, "; charset=utf-8"/utf8>>;

                <<"text/"/utf8, _/binary>> ->
                    <<Mime_type/binary, "; charset=utf-8"/utf8>>;

                _ ->
                    Mime_type
            end,
            case simplifile_erl:is_file(Path@1) of
                {ok, true} ->
                    _pipe@7 = gleam@http@response:new(200),
                    _pipe@8 = gleam@http@response:set_header(
                        _pipe@7,
                        <<"content-type"/utf8>>,
                        Content_type
                    ),
                    gleam@http@response:set_body(_pipe@8, {file, Path@1});

                _ ->
                    Handler()
            end;

        {_, _} ->
            Handler()
    end.

-spec handle_head(
    gleam@http@request:request(wisp@internal:connection()),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
handle_head(Req, Handler) ->
    case erlang:element(2, Req) of
        head ->
            _pipe = Req,
            _pipe@1 = gleam@http@request:set_method(_pipe, get),
            _pipe@2 = gleam@http@request:prepend_header(
                _pipe@1,
                <<"x-original-method"/utf8>>,
                <<"HEAD"/utf8>>
            ),
            _pipe@3 = Handler(_pipe@2),
            gleam@http@response:set_body(_pipe@3, empty);

        _ ->
            Handler(Req)
    end.

-spec new_temporary_file(gleam@http@request:request(wisp@internal:connection())) -> {ok,
        binary()} |
    {error, simplifile:file_error()}.
new_temporary_file(Request) ->
    Directory = erlang:element(7, erlang:element(4, Request)),
    gleam@result:'try'(
        simplifile:create_directory_all(Directory),
        fun(_) ->
            Path = wisp@internal:join_path(
                Directory,
                wisp@internal:random_slug()
            ),
            gleam@result:map(simplifile:create_file(Path), fun(_) -> Path end)
        end
    ).

-spec delete_temporary_files(
    gleam@http@request:request(wisp@internal:connection())
) -> {ok, nil} | {error, simplifile:file_error()}.
delete_temporary_files(Request) ->
    case simplifile_erl:delete(erlang:element(7, erlang:element(4, Request))) of
        {error, enoent} ->
            {ok, nil};

        Other ->
            Other
    end.

-spec configure_logger() -> nil.
configure_logger() ->
    logging_ffi:configure().

-spec log_level_to_logging_log_level(log_level()) -> logging:log_level().
log_level_to_logging_log_level(Log_level) ->
    case Log_level of
        emergency_level ->
            emergency;

        alert_level ->
            alert;

        critical_level ->
            critical;

        error_level ->
            error;

        warning_level ->
            warning;

        notice_level ->
            notice;

        info_level ->
            info;

        debug_level ->
            debug
    end.

-spec set_logger_level(log_level()) -> nil.
set_logger_level(Log_level) ->
    logging:set_level(log_level_to_logging_log_level(Log_level)).

-spec log_emergency(binary()) -> nil.
log_emergency(Message) ->
    logging:log(emergency, Message).

-spec log_alert(binary()) -> nil.
log_alert(Message) ->
    logging:log(alert, Message).

-spec log_critical(binary()) -> nil.
log_critical(Message) ->
    logging:log(critical, Message).

-spec log_error(binary()) -> nil.
log_error(Message) ->
    logging:log(error, Message).

-spec or_500({ok, ACNT} | {error, any()}) -> {ok, ACNT} |
    {error, gleam@http@response:response(body())}.
or_500(Result) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            log_error(gleam@string:inspect(Error)),
            {error, internal_server_error()}
    end.

-spec multipart_file_append(binary(), bitstring()) -> {ok, binary()} |
    {error, gleam@http@response:response(body())}.
multipart_file_append(Path, Chunk) ->
    _pipe = simplifile_erl:append_bits(Path, Chunk),
    _pipe@1 = or_500(_pipe),
    gleam@result:replace(_pipe@1, Path).

-spec read_multipart(
    gleam@http@request:request(wisp@internal:connection()),
    buffered_reader(),
    binary(),
    quotas(),
    form_data()
) -> {ok, form_data()} | {error, gleam@http@response:response(body())}.
read_multipart(Request, Reader, Boundary, Quotas, Data) ->
    Read_size = erlang:element(5, erlang:element(4, Request)),
    Header_parser = fn_with_bad_request_error(
        fun(_capture) ->
            gleam@http:parse_multipart_headers(_capture, Boundary)
        end
    ),
    Result = multipart_headers(Reader, Header_parser, Read_size, Quotas),
    gleam@result:'try'(
        Result,
        fun(_use0) ->
            {Headers, Reader@1, Quotas@1} = _use0,
            gleam@result:'try'(
                multipart_content_disposition(Headers),
                fun(_use0@1) ->
                    {Name, Filename} = _use0@1,
                    Parse = fn_with_bad_request_error(
                        fun(_capture@1) ->
                            gleam@http:parse_multipart_body(
                                _capture@1,
                                Boundary
                            )
                        end
                    ),
                    gleam@result:'try'(case Filename of
                            {some, File_name} ->
                                gleam@result:'try'(
                                    or_500(new_temporary_file(Request)),
                                    fun(Path) ->
                                        Append = fun multipart_file_append/2,
                                        Q = erlang:element(3, Quotas@1),
                                        Result@1 = multipart_body(
                                            Reader@1,
                                            Parse,
                                            Boundary,
                                            Read_size,
                                            Q,
                                            Append,
                                            Path
                                        ),
                                        gleam@result:map(
                                            Result@1,
                                            fun(_use0@2) ->
                                                {Reader@2, Quota, _} = _use0@2,
                                                Quotas@2 = erlang:setelement(
                                                    3,
                                                    Quotas@1,
                                                    Quota
                                                ),
                                                File = {uploaded_file,
                                                    File_name,
                                                    Path},
                                                Data@1 = erlang:setelement(
                                                    3,
                                                    Data,
                                                    [{Name, File} |
                                                        erlang:element(3, Data)]
                                                ),
                                                {Data@1, Reader@2, Quotas@2}
                                            end
                                        )
                                    end
                                );

                            none ->
                                Append@1 = fun(Data@2, Chunk) ->
                                    {ok, gleam@bit_array:append(Data@2, Chunk)}
                                end,
                                Q@1 = erlang:element(2, Quotas@1),
                                Result@2 = multipart_body(
                                    Reader@1,
                                    Parse,
                                    Boundary,
                                    Read_size,
                                    Q@1,
                                    Append@1,
                                    <<>>
                                ),
                                gleam@result:'try'(
                                    Result@2,
                                    fun(_use0@3) ->
                                        {Reader@3, Quota@1, Value} = _use0@3,
                                        Quotas@3 = erlang:setelement(
                                            2,
                                            Quotas@1,
                                            Quota@1
                                        ),
                                        gleam@result:map(
                                            bit_array_to_string(Value),
                                            fun(Value@1) ->
                                                Data@3 = erlang:setelement(
                                                    2,
                                                    Data,
                                                    [{Name, Value@1} |
                                                        erlang:element(2, Data)]
                                                ),
                                                {Data@3, Reader@3, Quotas@3}
                                            end
                                        )
                                    end
                                )
                        end, fun(_use0@4) ->
                            {Data@4, Reader@4, Quotas@4} = _use0@4,
                            case Reader@4 of
                                {some, Reader@5} ->
                                    read_multipart(
                                        Request,
                                        Reader@5,
                                        Boundary,
                                        Quotas@4,
                                        Data@4
                                    );

                                none ->
                                    {ok,
                                        {form_data,
                                            sort_keys(erlang:element(2, Data@4)),
                                            sort_keys(erlang:element(3, Data@4))}}
                            end
                        end)
                end
            )
        end
    ).

-spec require_multipart_form(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    fun((form_data()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_multipart_form(Request, Boundary, Next) ->
    Quotas = {quotas,
        erlang:element(3, erlang:element(4, Request)),
        erlang:element(4, erlang:element(4, Request))},
    Reader = {buffered_reader,
        erlang:element(2, erlang:element(4, Request)),
        <<>>},
    Result = read_multipart(
        Request,
        Reader,
        Boundary,
        Quotas,
        {form_data, [], []}
    ),
    case Result of
        {ok, Form_data} ->
            Next(Form_data);

        {error, Response} ->
            Response
    end.

-spec require_form(
    gleam@http@request:request(wisp@internal:connection()),
    fun((form_data()) -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
require_form(Request, Next) ->
    case gleam@list:key_find(
        erlang:element(3, Request),
        <<"content-type"/utf8>>
    ) of
        {ok, <<"application/x-www-form-urlencoded"/utf8>>} ->
            require_urlencoded_form(Request, Next);

        {ok, <<"application/x-www-form-urlencoded;"/utf8, _/binary>>} ->
            require_urlencoded_form(Request, Next);

        {ok, <<"multipart/form-data; boundary="/utf8, Boundary/binary>>} ->
            require_multipart_form(Request, Boundary, Next);

        {ok, <<"multipart/form-data"/utf8>>} ->
            bad_request();

        _ ->
            unsupported_media_type(
                [<<"application/x-www-form-urlencoded"/utf8>>,
                    <<"multipart/form-data"/utf8>>]
            )
    end.

-spec rescue_crashes(fun(() -> gleam@http@response:response(body()))) -> gleam@http@response:response(body()).
rescue_crashes(Handler) ->
    case exception_ffi:rescue(Handler) of
        {ok, Response} ->
            Response;

        {error, Error} ->
            {Kind, Detail@3} = case Error of
                {errored, Detail} ->
                    {errored, Detail};

                {thrown, Detail@1} ->
                    {thrown, Detail@1};

                {exited, Detail@2} ->
                    {exited, Detail@2}
            end,
            case (gleam@dynamic:dict(
                fun gleam_erlang_ffi:atom_from_dynamic/1,
                fun(Field@0) -> {ok, Field@0} end
            ))(Detail@3) of
                {ok, Details} ->
                    C = erlang:binary_to_atom(<<"class"/utf8>>),
                    logger:error(
                        gleam@dict:insert(Details, C, gleam@dynamic:from(Kind))
                    ),
                    nil;

                {error, _} ->
                    log_error(gleam@string:inspect(Error))
            end,
            internal_server_error()
    end.

-spec log_warning(binary()) -> nil.
log_warning(Message) ->
    logging:log(warning, Message).

-spec log_notice(binary()) -> nil.
log_notice(Message) ->
    logging:log(notice, Message).

-spec log_info(binary()) -> nil.
log_info(Message) ->
    logging:log(info, Message).

-spec log_request(
    gleam@http@request:request(wisp@internal:connection()),
    fun(() -> gleam@http@response:response(body()))
) -> gleam@http@response:response(body()).
log_request(Req, Handler) ->
    Response = Handler(),
    _pipe = [gleam@int:to_string(erlang:element(2, Response)),
        <<" "/utf8>>,
        gleam@string:uppercase(
            gleam@http:method_to_string(erlang:element(2, Req))
        ),
        <<" "/utf8>>,
        erlang:element(8, Req)],
    _pipe@1 = gleam@string:concat(_pipe),
    log_info(_pipe@1),
    Response.

-spec log_debug(binary()) -> nil.
log_debug(Message) ->
    logging:log(debug, Message).

-spec random_string(integer()) -> binary().
random_string(Length) ->
    wisp@internal:random_string(Length).

-spec sign_message(
    gleam@http@request:request(wisp@internal:connection()),
    bitstring(),
    gleam@crypto:hash_algorithm()
) -> binary().
sign_message(Request, Message, Algorithm) ->
    gleam@crypto:sign_message(
        Message,
        <<(erlang:element(6, erlang:element(4, Request)))/binary>>,
        Algorithm
    ).

-spec verify_signed_message(
    gleam@http@request:request(wisp@internal:connection()),
    binary()
) -> {ok, bitstring()} | {error, nil}.
verify_signed_message(Request, Message) ->
    gleam@crypto:verify_signed_message(
        Message,
        <<(erlang:element(6, erlang:element(4, Request)))/binary>>
    ).

-spec set_cookie(
    gleam@http@response:response(body()),
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    binary(),
    security(),
    integer()
) -> gleam@http@response:response(body()).
set_cookie(Response, Request, Name, Value, Security, Max_age) ->
    Attributes = erlang:setelement(
        2,
        gleam@http@cookie:defaults(https),
        {some, Max_age}
    ),
    Value@1 = case Security of
        plain_text ->
            gleam_stdlib:bit_array_base64_encode(<<Value/binary>>, false);

        signed ->
            sign_message(Request, <<Value/binary>>, sha512)
    end,
    _pipe = Response,
    gleam@http@response:set_cookie(_pipe, Name, Value@1, Attributes).

-spec get_cookie(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    security()
) -> {ok, binary()} | {error, nil}.
get_cookie(Request, Name, Security) ->
    gleam@result:'try'(
        begin
            _pipe = Request,
            _pipe@1 = gleam@http@request:get_cookies(_pipe),
            gleam@list:key_find(_pipe@1, Name)
        end,
        fun(Value) -> gleam@result:'try'(case Security of
                    plain_text ->
                        gleam@bit_array:base64_decode(Value);

                    signed ->
                        verify_signed_message(Request, Value)
                end, fun(Value@1) -> gleam@bit_array:to_string(Value@1) end) end
    ).

-spec create_canned_connection(bitstring(), binary()) -> wisp@internal:connection().
create_canned_connection(Body, Secret_key_base) ->
    wisp@internal:make_connection(
        fun(_) -> {ok, {chunk, Body, fun(_) -> {ok, reading_finished} end}} end,
        Secret_key_base
    ).

-spec do_escape_html_regular(
    bitstring(),
    integer(),
    bitstring(),
    list(bitstring()),
    integer()
) -> list(bitstring()).
do_escape_html_regular(Bin, Skip, Original, Acc, Len) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            _assert_subject = gleam_stdlib:bit_array_slice(Original, Skip, Len),
            {ok, Slice} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"wisp"/utf8>>,
                                function => <<"do_escape_html_regular"/utf8>>,
                                line => 403})
            end,
            Acc@1 = [<<"&lt;"/utf8>>, Slice | Acc],
            do_escape_html(Rest, (Skip + Len) + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            _assert_subject@1 = gleam_stdlib:bit_array_slice(
                Original,
                Skip,
                Len
            ),
            {ok, Slice@1} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"wisp"/utf8>>,
                                function => <<"do_escape_html_regular"/utf8>>,
                                line => 409})
            end,
            Acc@2 = [<<"&gt;"/utf8>>, Slice@1 | Acc],
            do_escape_html(Rest@1, (Skip + Len) + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            _assert_subject@2 = gleam_stdlib:bit_array_slice(
                Original,
                Skip,
                Len
            ),
            {ok, Slice@2} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"wisp"/utf8>>,
                                function => <<"do_escape_html_regular"/utf8>>,
                                line => 415})
            end,
            Acc@3 = [<<"&amp;"/utf8>>, Slice@2 | Acc],
            do_escape_html(Rest@2, (Skip + Len) + 1, Original, Acc@3);

        <<_, Rest@3/bitstring>> ->
            do_escape_html_regular(Rest@3, Skip, Original, Acc, Len + 1);

        <<>> ->
            case Skip of
                0 ->
                    [Original];

                _ ->
                    _assert_subject@3 = gleam_stdlib:bit_array_slice(
                        Original,
                        Skip,
                        Len
                    ),
                    {ok, Slice@3} = case _assert_subject@3 of
                        {ok, _} -> _assert_subject@3;
                        _assert_fail@3 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@3,
                                        module => <<"wisp"/utf8>>,
                                        function => <<"do_escape_html_regular"/utf8>>,
                                        line => 429})
                    end,
                    [Slice@3 | Acc]
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"non byte aligned string, all strings should be byte aligned"/utf8>>,
                    module => <<"wisp"/utf8>>,
                    function => <<"do_escape_html_regular"/utf8>>,
                    line => 434})
    end.

-spec do_escape_html(bitstring(), integer(), bitstring(), list(bitstring())) -> list(bitstring()).
do_escape_html(Bin, Skip, Original, Acc) ->
    case Bin of
        <<"<"/utf8, Rest/bitstring>> ->
            Acc@1 = [<<"&lt;"/utf8>> | Acc],
            do_escape_html(Rest, Skip + 1, Original, Acc@1);

        <<">"/utf8, Rest@1/bitstring>> ->
            Acc@2 = [<<"&gt;"/utf8>> | Acc],
            do_escape_html(Rest@1, Skip + 1, Original, Acc@2);

        <<"&"/utf8, Rest@2/bitstring>> ->
            Acc@3 = [<<"&amp;"/utf8>> | Acc],
            do_escape_html(Rest@2, Skip + 1, Original, Acc@3);

        <<_, Rest@3/bitstring>> ->
            do_escape_html_regular(Rest@3, Skip, Original, Acc, 1);

        <<>> ->
            Acc;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"non byte aligned string, all strings should be byte aligned"/utf8>>,
                    module => <<"wisp"/utf8>>,
                    function => <<"do_escape_html"/utf8>>,
                    line => 368})
    end.

-spec escape_html(binary()) -> binary().
escape_html(Content) ->
    Bits = <<Content/binary>>,
    Acc = do_escape_html(Bits, 0, Bits, []),
    _pipe = lists:reverse(Acc),
    _pipe@1 = gleam_stdlib:bit_array_concat(_pipe),
    wisp_ffi:coerce(_pipe@1).
