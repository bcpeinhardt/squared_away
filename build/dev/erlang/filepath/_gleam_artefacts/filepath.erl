-module(filepath).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([join/2, split_unix/1, split_windows/1, split/1, base_name/1, extension/1, strip_extension/1, directory_name/1, is_absolute/1, expand/1]).

-spec relative(binary()) -> binary().
relative(Path) ->
    case Path of
        <<"/"/utf8, Path@1/binary>> ->
            relative(Path@1);

        _ ->
            Path
    end.

-spec remove_trailing_slash(binary()) -> binary().
remove_trailing_slash(Path) ->
    case gleam@string:ends_with(Path, <<"/"/utf8>>) of
        true ->
            gleam@string:drop_right(Path, 1);

        false ->
            Path
    end.

-spec join(binary(), binary()) -> binary().
join(Left, Right) ->
    _pipe@2 = case {Left, Right} of
        {_, <<"/"/utf8>>} ->
            Left;

        {<<""/utf8>>, _} ->
            relative(Right);

        {<<"/"/utf8>>, _} ->
            Right;

        {_, _} ->
            _pipe = remove_trailing_slash(Left),
            _pipe@1 = gleam@string:append(_pipe, <<"/"/utf8>>),
            gleam@string:append(_pipe@1, relative(Right))
    end,
    remove_trailing_slash(_pipe@2).

-spec split_unix(binary()) -> list(binary()).
split_unix(Path) ->
    _pipe = case gleam@string:split(Path, <<"/"/utf8>>) of
        [<<""/utf8>>] ->
            [];

        [<<""/utf8>> | Rest] ->
            [<<"/"/utf8>> | Rest];

        Rest@1 ->
            Rest@1
    end,
    gleam@list:filter(_pipe, fun(X) -> X /= <<""/utf8>> end).

-spec pop_windows_drive_specifier(binary()) -> {gleam@option:option(binary()),
    binary()}.
pop_windows_drive_specifier(Path) ->
    Start = gleam@string:slice(Path, 0, 3),
    Codepoints = gleam@string:to_utf_codepoints(Start),
    case gleam@list:map(Codepoints, fun gleam@string:utf_codepoint_to_int/1) of
        [Drive, Colon, Slash] when (((Slash =:= 47) orelse (Slash =:= 92)) andalso (Colon =:= 58)) andalso (((Drive >= 65) andalso (Drive =< 90)) orelse ((Drive >= 97) andalso (Drive =< 122))) ->
            Drive_letter = gleam@string:slice(Path, 0, 1),
            Drive@1 = <<(gleam@string:lowercase(Drive_letter))/binary,
                ":/"/utf8>>,
            Path@1 = gleam@string:drop_left(Path, 3),
            {{some, Drive@1}, Path@1};

        _ ->
            {none, Path}
    end.

-spec split_windows(binary()) -> list(binary()).
split_windows(Path) ->
    {Drive, Path@1} = pop_windows_drive_specifier(Path),
    Segments = begin
        _pipe = gleam@string:split(Path@1, <<"/"/utf8>>),
        gleam@list:flat_map(
            _pipe,
            fun(_capture) -> gleam@string:split(_capture, <<"\\"/utf8>>) end
        )
    end,
    Segments@1 = case Drive of
        {some, Drive@1} ->
            [Drive@1 | Segments];

        none ->
            Segments
    end,
    case Segments@1 of
        [<<""/utf8>>] ->
            [];

        [<<""/utf8>> | Rest] ->
            [<<"/"/utf8>> | Rest];

        Rest@1 ->
            Rest@1
    end.

-spec split(binary()) -> list(binary()).
split(Path) ->
    case filepath_ffi:is_windows() of
        true ->
            split_windows(Path);

        false ->
            split_unix(Path)
    end.

-spec base_name(binary()) -> binary().
base_name(Path) ->
    gleam@bool:guard(Path =:= <<"/"/utf8>>, <<""/utf8>>, fun() -> _pipe = Path,
            _pipe@1 = split(_pipe),
            _pipe@2 = gleam@list:last(_pipe@1),
            gleam@result:unwrap(_pipe@2, <<""/utf8>>) end).

-spec extension(binary()) -> {ok, binary()} | {error, nil}.
extension(Path) ->
    File = base_name(Path),
    case gleam@string:split(File, <<"."/utf8>>) of
        [<<""/utf8>>, _] ->
            {error, nil};

        [_, Extension] ->
            {ok, Extension};

        [_ | Rest] ->
            gleam@list:last(Rest);

        _ ->
            {error, nil}
    end.

-spec strip_extension(binary()) -> binary().
strip_extension(Path) ->
    case extension(Path) of
        {ok, Extension} ->
            gleam@string:drop_right(Path, gleam@string:length(Extension) + 1);

        {error, nil} ->
            Path
    end.

-spec get_directory_name(list(binary()), binary(), binary()) -> binary().
get_directory_name(Path, Acc, Segment) ->
    case Path of
        [<<"/"/utf8>> | Rest] ->
            get_directory_name(
                Rest,
                <<Acc/binary, Segment/binary>>,
                <<"/"/utf8>>
            );

        [First | Rest@1] ->
            get_directory_name(Rest@1, Acc, <<Segment/binary, First/binary>>);

        [] ->
            Acc
    end.

-spec directory_name(binary()) -> binary().
directory_name(Path) ->
    Path@1 = remove_trailing_slash(Path),
    case Path@1 of
        <<"/"/utf8, Rest/binary>> ->
            get_directory_name(
                gleam@string:to_graphemes(Rest),
                <<"/"/utf8>>,
                <<""/utf8>>
            );

        _ ->
            get_directory_name(
                gleam@string:to_graphemes(Path@1),
                <<""/utf8>>,
                <<""/utf8>>
            )
    end.

-spec is_absolute(binary()) -> boolean().
is_absolute(Path) ->
    gleam@string:starts_with(Path, <<"/"/utf8>>).

-spec expand_segments(list(binary()), list(binary())) -> {ok, binary()} |
    {error, nil}.
expand_segments(Path, Base) ->
    case {Base, Path} of
        {[<<""/utf8>>], [<<".."/utf8>> | _]} ->
            {error, nil};

        {[], [<<".."/utf8>> | _]} ->
            {error, nil};

        {[_ | Base@1], [<<".."/utf8>> | Path@1]} ->
            expand_segments(Path@1, Base@1);

        {_, [<<"."/utf8>> | Path@2]} ->
            expand_segments(Path@2, Base);

        {_, [S | Path@3]} ->
            expand_segments(Path@3, [S | Base]);

        {_, []} ->
            {ok, gleam@string:join(lists:reverse(Base), <<"/"/utf8>>)}
    end.

-spec root_slash_to_empty(list(binary())) -> list(binary()).
root_slash_to_empty(Segments) ->
    case Segments of
        [<<"/"/utf8>> | Rest] ->
            [<<""/utf8>> | Rest];

        _ ->
            Segments
    end.

-spec expand(binary()) -> {ok, binary()} | {error, nil}.
expand(Path) ->
    Is_absolute = is_absolute(Path),
    Result = begin
        _pipe = Path,
        _pipe@1 = split(_pipe),
        _pipe@2 = root_slash_to_empty(_pipe@1),
        expand_segments(_pipe@2, [])
    end,
    case Is_absolute andalso (Result =:= {ok, <<""/utf8>>}) of
        true ->
            {ok, <<"/"/utf8>>};

        false ->
            Result
    end.
