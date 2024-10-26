-module(wisp@internal).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([remove_preceeding_slashes/1, join_path/2, random_string/1, random_slug/0, make_connection/2]).
-export_type([connection/0, read/0]).

-type connection() :: {connection,
        fun((integer()) -> {ok, read()} | {error, nil}),
        integer(),
        integer(),
        integer(),
        binary(),
        binary()}.

-type read() :: {chunk,
        bitstring(),
        fun((integer()) -> {ok, read()} | {error, nil})} |
    reading_finished.

-file("/Users/louis/src/gleam/wisp/src/wisp/internal.gleam", 60).
-spec remove_preceeding_slashes(binary()) -> binary().
remove_preceeding_slashes(String) ->
    case String of
        <<"/"/utf8, Rest/binary>> ->
            remove_preceeding_slashes(Rest);

        _ ->
            String
    end.

-file("/Users/louis/src/gleam/wisp/src/wisp/internal.gleam", 68).
-spec join_path(binary(), binary()) -> binary().
join_path(A, B) ->
    B@1 = remove_preceeding_slashes(B),
    case gleam@string:ends_with(A, <<"/"/utf8>>) of
        true ->
            <<A/binary, B@1/binary>>;

        false ->
            <<<<A/binary, "/"/utf8>>/binary, B@1/binary>>
    end.

-file("/Users/louis/src/gleam/wisp/src/wisp/internal.gleam", 82).
-spec random_string(integer()) -> binary().
random_string(Length) ->
    _pipe = crypto:strong_rand_bytes(Length),
    _pipe@1 = gleam@bit_array:base64_url_encode(_pipe, false),
    gleam@string:slice(_pipe@1, 0, Length).

-file("/Users/louis/src/gleam/wisp/src/wisp/internal.gleam", 88).
-spec random_slug() -> binary().
random_slug() ->
    random_string(16).

-file("/Users/louis/src/gleam/wisp/src/wisp/internal.gleam", 28).
-spec make_connection(fun((integer()) -> {ok, read()} | {error, nil}), binary()) -> connection().
make_connection(Body_reader, Secret_key_base) ->
    Prefix = case directories:tmp_dir() of
        {ok, Tmp_dir} ->
            <<Tmp_dir/binary, "/gleam-wisp/"/utf8>>;

        {error, _} ->
            <<"./tmp/"/utf8>>
    end,
    Temporary_directory = join_path(Prefix, random_slug()),
    {connection,
        Body_reader,
        8000000,
        32000000,
        1000000,
        Secret_key_base,
        Temporary_directory}.
