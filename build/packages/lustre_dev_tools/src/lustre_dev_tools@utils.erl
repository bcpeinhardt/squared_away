-module(lustre_dev_tools@utils).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([term_width/0, shorten_url/2]).
-export_type(['end'/0]).

-type 'end'() :: left | right.

-spec term_width() -> integer().
term_width() ->
    _pipe = term_size:columns(),
    gleam@result:unwrap(_pipe, 80).

-spec do_shorten(
    list(binary()),
    list(binary()),
    boolean(),
    'end'(),
    integer(),
    integer()
) -> {ok, {list(binary()), list(binary())}} | {error, nil}.
do_shorten(Left, Right, Shortened, From, Current_length, Max_length) ->
    case {Current_length =< Max_length, Left, Right, From} of
        {true, _, _, _} ->
            case Shortened of
                true ->
                    {ok, {Left, Right}};

                false ->
                    {error, nil}
            end;

        {_, [], [_], _} ->
            case Shortened of
                true ->
                    {ok, {Left, Right}};

                false ->
                    {error, nil}
            end;

        {_, [], [], _} ->
            case Shortened of
                true ->
                    {ok, {Left, Right}};

                false ->
                    {error, nil}
            end;

        {_, Left@1, [_] = Right@1, right} ->
            do_shorten(
                Left@1,
                Right@1,
                Shortened,
                left,
                Current_length,
                Max_length
            );

        {_, [Dropped | Left@2], Right@2, left} ->
            New_length = Current_length - gleam@string:length(Dropped),
            do_shorten(Left@2, Right@2, true, right, New_length, Max_length);

        {_, Left@3, [Dropped@1 | Right@3], right} ->
            New_length@1 = Current_length - gleam@string:length(Dropped@1),
            do_shorten(Left@3, Right@3, true, left, New_length@1, Max_length);

        {_, [], Right@4, left} ->
            do_shorten(
                [],
                Right@4,
                Shortened,
                right,
                Current_length,
                Max_length
            );

        {_, Left@4, [], right} ->
            do_shorten(Left@4, [], Shortened, left, Current_length, Max_length)
    end.

-spec shorten(list(binary()), integer()) -> {ok,
        {list(binary()), list(binary())}} |
    {error, nil}.
shorten(Strings, Max_length) ->
    Initial_length = gleam@list:fold(
        Strings,
        0,
        fun(Acc, String) -> Acc + gleam@string:length(String) end
    ),
    Middle = erlang:length(Strings) div 2,
    {Left, Right} = gleam@list:split(Strings, Middle),
    Left@1 = lists:reverse(Left),
    case do_shorten(Left@1, Right, false, right, Initial_length, Max_length) of
        {ok, {New_left, New_right}} ->
            {ok, {lists:reverse(New_left), New_right}};

        {error, nil} ->
            {error, nil}
    end.

-spec shorten_url(binary(), integer()) -> binary().
shorten_url(Url, Max_length) ->
    Chunks = case Url of
        <<"https://"/utf8, Rest/binary>> ->
            [<<"https:/"/utf8>> | gleam@string:split(Rest, <<"/"/utf8>>)];

        _ ->
            gleam@string:split(Url, <<"/"/utf8>>)
    end,
    Max_length@1 = Max_length - erlang:length(Chunks),
    case shorten(Chunks, Max_length@1) of
        {error, _} ->
            Url;

        {ok, {Left, Right}} ->
            <<<<(gleam@string:join(Left, <<"/"/utf8>>))/binary, "/.../"/utf8>>/binary,
                (gleam@string:join(Right, <<"/"/utf8>>))/binary>>
    end.
