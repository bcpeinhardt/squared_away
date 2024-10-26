-module(birl@interval).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_start_and_end/2, from_start_and_duration/2, shift/2, scale_up/2, scale_down/2, includes/2, contains/2, intersection/2, get_bounds/1]).
-export_type([interval/0]).

-opaque interval() :: {interval, birl:time(), birl:time()}.

-spec from_start_and_end(birl:time(), birl:time()) -> {ok, interval()} |
    {error, nil}.
from_start_and_end(Start, End) ->
    case birl:compare(Start, End) of
        eq ->
            {error, nil};

        lt ->
            {ok, {interval, Start, End}};

        gt ->
            {ok, {interval, End, Start}}
    end.

-spec from_start_and_duration(birl:time(), birl@duration:duration()) -> {ok,
        interval()} |
    {error, nil}.
from_start_and_duration(Start, Duration) ->
    from_start_and_end(Start, birl:add(Start, Duration)).

-spec shift(interval(), birl@duration:duration()) -> interval().
shift(Interval, Duration) ->
    case Interval of
        {interval, Start, End} ->
            {interval, birl:add(Start, Duration), birl:add(End, Duration)}
    end.

-spec scale_up(interval(), integer()) -> interval().
scale_up(Interval, Factor) ->
    case Interval of
        {interval, Start, End} ->
            _assert_subject = begin
                _pipe = birl:difference(End, Start),
                _pipe@1 = birl@duration:scale_up(_pipe, Factor),
                from_start_and_duration(Start, _pipe@1)
            end,
            {ok, Interval@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"birl/interval"/utf8>>,
                                function => <<"scale_up"/utf8>>,
                                line => 34})
            end,
            Interval@1
    end.

-spec scale_down(interval(), integer()) -> interval().
scale_down(Interval, Factor) ->
    case Interval of
        {interval, Start, End} ->
            _assert_subject = begin
                _pipe = birl:difference(End, Start),
                _pipe@1 = birl@duration:scale_down(_pipe, Factor),
                from_start_and_duration(Start, _pipe@1)
            end,
            {ok, Interval@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"birl/interval"/utf8>>,
                                function => <<"scale_down"/utf8>>,
                                line => 46})
            end,
            Interval@1
    end.

-spec includes(interval(), birl:time()) -> boolean().
includes(Interval, Time) ->
    case Interval of
        {interval, Start, End} ->
            gleam@list:contains([eq, lt], birl:compare(Start, Time)) andalso gleam@list:contains(
                [eq, gt],
                birl:compare(End, Time)
            )
    end.

-spec contains(interval(), interval()) -> boolean().
contains(A, B) ->
    case B of
        {interval, Start, End} ->
            includes(A, Start) andalso includes(A, End)
    end.

-spec intersection(interval(), interval()) -> gleam@option:option(interval()).
intersection(A, B) ->
    case {contains(A, B), contains(B, A)} of
        {true, false} ->
            {some, B};

        {false, true} ->
            {some, A};

        {_, _} ->
            {interval, A_start, A_end} = A,
            {interval, B_start, B_end} = B,
            case {includes(A, B_start), includes(B, A_start)} of
                {true, false} ->
                    {some, {interval, B_start, A_end}};

                {false, true} ->
                    {some, {interval, A_start, B_end}};

                {_, _} ->
                    none
            end
    end.

-spec get_bounds(interval()) -> {birl:time(), birl:time()}.
get_bounds(Interval) ->
    case Interval of
        {interval, Start, End} ->
            {Start, End}
    end.
