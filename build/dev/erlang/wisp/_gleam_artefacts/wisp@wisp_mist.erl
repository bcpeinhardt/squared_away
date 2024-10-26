-module(wisp@wisp_mist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([handler/2]).

-spec wrap_mist_chunk({ok, mist:chunk()} | {error, mist:read_error()}) -> {ok,
        wisp@internal:read()} |
    {error, nil}.
wrap_mist_chunk(Chunk) ->
    _pipe = Chunk,
    _pipe@1 = gleam@result:nil_error(_pipe),
    gleam@result:map(_pipe@1, fun(Chunk@1) -> case Chunk@1 of
                done ->
                    reading_finished;

                {chunk, Data, Consume} ->
                    {chunk,
                        Data,
                        fun(Size) -> wrap_mist_chunk(Consume(Size)) end}
            end end).

-spec mist_body_reader(
    gleam@http@request:request(mist@internal@http:connection())
) -> fun((integer()) -> {ok, wisp@internal:read()} | {error, nil}).
mist_body_reader(Request) ->
    case mist:stream(Request) of
        {error, _} ->
            fun(_) -> {ok, reading_finished} end;

        {ok, Stream} ->
            fun(Size) -> wrap_mist_chunk(Stream(Size)) end
    end.

-spec mist_send_file(binary()) -> mist:response_data().
mist_send_file(Path) ->
    case mist:send_file(Path, 0, none) of
        {ok, Body} ->
            Body;

        {error, Error} ->
            wisp:log_error(gleam@string:inspect(Error)),
            {bytes, gleam@bytes_builder:new()}
    end.

-spec mist_response(gleam@http@response:response(wisp:body())) -> gleam@http@response:response(mist:response_data()).
mist_response(Response) ->
    Body = case erlang:element(4, Response) of
        empty ->
            {bytes, gleam@bytes_builder:new()};

        {text, Text} ->
            {bytes, gleam_stdlib:wrap_list(Text)};

        {bytes, Bytes} ->
            {bytes, Bytes};

        {file, Path} ->
            mist_send_file(Path)
    end,
    _pipe = Response,
    gleam@http@response:set_body(_pipe, Body).

-spec handler(
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body())),
    binary()
) -> fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data())).
handler(Handler, Secret_key_base) ->
    fun(Request) ->
        Connection = wisp@internal:make_connection(
            mist_body_reader(Request),
            Secret_key_base
        ),
        Request@1 = gleam@http@request:set_body(Request, Connection),
        exception_ffi:defer(
            fun() -> _assert_subject = wisp:delete_temporary_files(Request@1),
                {ok, _} = case _assert_subject of
                    {ok, _} -> _assert_subject;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Assertion pattern match failed"/utf8>>,
                                    value => _assert_fail,
                                    module => <<"wisp/wisp_mist"/utf8>>,
                                    function => <<"handler"/utf8>>,
                                    line => 43})
                end end,
            fun() ->
                Response = begin
                    _pipe = Request@1,
                    _pipe@1 = Handler(_pipe),
                    mist_response(_pipe@1)
                end,
                Response
            end
        )
    end.
