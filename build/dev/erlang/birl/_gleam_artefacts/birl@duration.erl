-module(birl@duration).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([add/2, subtract/2, scale_up/2, scale_down/2, micro_seconds/1, compare/2, milli_seconds/1, seconds/1, minutes/1, hours/1, days/1, weeks/1, months/1, years/1, new/1, decompose/1, accurate_new/1, accurate_decompose/1, blur_to/2, blur/1, parse/1]).
-export_type([duration/0, unit/0]).

-type duration() :: {duration, integer()}.

-type unit() :: micro_second |
    milli_second |
    second |
    minute |
    hour |
    day |
    week |
    month |
    year.

-spec add(duration(), duration()) -> duration().
add(A, B) ->
    {duration, A@1} = A,
    {duration, B@1} = B,
    {duration, A@1 + B@1}.

-spec subtract(duration(), duration()) -> duration().
subtract(A, B) ->
    {duration, A@1} = A,
    {duration, B@1} = B,
    {duration, A@1 - B@1}.

-spec scale_up(duration(), integer()) -> duration().
scale_up(Value, Factor) ->
    {duration, Value@1} = Value,
    {duration, Value@1 * Factor}.

-spec scale_down(duration(), integer()) -> duration().
scale_down(Value, Factor) ->
    {duration, Value@1} = Value,
    {duration, case Factor of
            0 -> 0;
            Gleam@denominator -> Value@1 div Gleam@denominator
        end}.

-spec micro_seconds(integer()) -> duration().
micro_seconds(Value) ->
    {duration, Value}.

-spec compare(duration(), duration()) -> gleam@order:order().
compare(A, B) ->
    {duration, Dta} = A,
    {duration, Dtb} = B,
    case {Dta =:= Dtb, Dta < Dtb} of
        {true, _} ->
            eq;

        {_, true} ->
            lt;

        {_, false} ->
            gt
    end.

-spec extract(integer(), integer()) -> {integer(), integer()}.
extract(Duration, Unit_value) ->
    {case Unit_value of
            0 -> 0;
            Gleam@denominator -> Duration div Gleam@denominator
        end, case Unit_value of
            0 -> 0;
            Gleam@denominator@1 -> Duration rem Gleam@denominator@1
        end}.

-spec milli_seconds(integer()) -> duration().
milli_seconds(Value) ->
    {duration, Value * 1000}.

-spec seconds(integer()) -> duration().
seconds(Value) ->
    {duration, Value * 1000000}.

-spec minutes(integer()) -> duration().
minutes(Value) ->
    {duration, Value * 60000000}.

-spec hours(integer()) -> duration().
hours(Value) ->
    {duration, Value * 3600000000}.

-spec days(integer()) -> duration().
days(Value) ->
    {duration, Value * 86400000000}.

-spec weeks(integer()) -> duration().
weeks(Value) ->
    {duration, Value * 604800000000}.

-spec months(integer()) -> duration().
months(Value) ->
    {duration, Value * 2592000000000}.

-spec years(integer()) -> duration().
years(Value) ->
    {duration, Value * 31536000000000}.

-spec new(list({integer(), unit()})) -> duration().
new(Values) ->
    _pipe = Values,
    _pipe@1 = gleam@list:fold(_pipe, 0, fun(Total, Current) -> case Current of
                {Amount, micro_second} ->
                    Total + Amount;

                {Amount@1, milli_second} ->
                    Total + (Amount@1 * 1000);

                {Amount@2, second} ->
                    Total + (Amount@2 * 1000000);

                {Amount@3, minute} ->
                    Total + (Amount@3 * 60000000);

                {Amount@4, hour} ->
                    Total + (Amount@4 * 3600000000);

                {Amount@5, day} ->
                    Total + (Amount@5 * 86400000000);

                {Amount@6, week} ->
                    Total + (Amount@6 * 604800000000);

                {Amount@7, month} ->
                    Total + (Amount@7 * 2592000000000);

                {Amount@8, year} ->
                    Total + (Amount@8 * 31536000000000)
            end end),
    {duration, _pipe@1}.

-spec decompose(duration()) -> list({integer(), unit()}).
decompose(Duration) ->
    {duration, Value} = Duration,
    Absolute_value = gleam@int:absolute_value(Value),
    {Years, Remaining} = extract(Absolute_value, 31536000000000),
    {Months, Remaining@1} = extract(Remaining, 2592000000000),
    {Weeks, Remaining@2} = extract(Remaining@1, 604800000000),
    {Days, Remaining@3} = extract(Remaining@2, 86400000000),
    {Hours, Remaining@4} = extract(Remaining@3, 3600000000),
    {Minutes, Remaining@5} = extract(Remaining@4, 60000000),
    {Seconds, Remaining@6} = extract(Remaining@5, 1000000),
    {Milli_seconds, Remaining@7} = extract(Remaining@6, 1000),
    _pipe = [{Years, year},
        {Months, month},
        {Weeks, week},
        {Days, day},
        {Hours, hour},
        {Minutes, minute},
        {Seconds, second},
        {Milli_seconds, milli_second},
        {Remaining@7, micro_second}],
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Item) -> erlang:element(1, Item) > 0 end
    ),
    gleam@list:map(_pipe@1, fun(Item@1) -> case Value < 0 of
                true ->
                    {-1 * erlang:element(1, Item@1), erlang:element(2, Item@1)};

                false ->
                    Item@1
            end end).

-spec accurate_new(list({integer(), unit()})) -> duration().
accurate_new(Values) ->
    _pipe = Values,
    _pipe@1 = gleam@list:fold(_pipe, 0, fun(Total, Current) -> case Current of
                {Amount, micro_second} ->
                    Total + Amount;

                {Amount@1, milli_second} ->
                    Total + (Amount@1 * 1000);

                {Amount@2, second} ->
                    Total + (Amount@2 * 1000000);

                {Amount@3, minute} ->
                    Total + (Amount@3 * 60000000);

                {Amount@4, hour} ->
                    Total + (Amount@4 * 3600000000);

                {Amount@5, day} ->
                    Total + (Amount@5 * 86400000000);

                {Amount@6, week} ->
                    Total + (Amount@6 * 604800000000);

                {Amount@7, month} ->
                    Total + (Amount@7 * 2629746000000);

                {Amount@8, year} ->
                    Total + (Amount@8 * 31556952000000)
            end end),
    {duration, _pipe@1}.

-spec accurate_decompose(duration()) -> list({integer(), unit()}).
accurate_decompose(Duration) ->
    {duration, Value} = Duration,
    Absolute_value = gleam@int:absolute_value(Value),
    {Years, Remaining} = extract(Absolute_value, 31556952000000),
    {Months, Remaining@1} = extract(Remaining, 2629746000000),
    {Weeks, Remaining@2} = extract(Remaining@1, 604800000000),
    {Days, Remaining@3} = extract(Remaining@2, 86400000000),
    {Hours, Remaining@4} = extract(Remaining@3, 3600000000),
    {Minutes, Remaining@5} = extract(Remaining@4, 60000000),
    {Seconds, Remaining@6} = extract(Remaining@5, 1000000),
    {Milli_seconds, Remaining@7} = extract(Remaining@6, 1000),
    _pipe = [{Years, year},
        {Months, month},
        {Weeks, week},
        {Days, day},
        {Hours, hour},
        {Minutes, minute},
        {Seconds, second},
        {Milli_seconds, milli_second},
        {Remaining@7, micro_second}],
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(Item) -> erlang:element(1, Item) > 0 end
    ),
    gleam@list:map(_pipe@1, fun(Item@1) -> case Value < 0 of
                true ->
                    {-1 * erlang:element(1, Item@1), erlang:element(2, Item@1)};

                false ->
                    Item@1
            end end).

-spec blur_to(duration(), unit()) -> integer().
blur_to(Duration, Unit) ->
    _assert_subject = gleam@list:key_find(
        [{year, 31536000000000},
            {month, 2592000000000},
            {week, 604800000000},
            {day, 86400000000},
            {hour, 3600000000},
            {minute, 60000000},
            {second, 1000000},
            {milli_second, 1000},
            {micro_second, 1}],
        Unit
    ),
    {ok, Unit_value} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl/duration"/utf8>>,
                        function => <<"blur_to"/utf8>>,
                        line => 207})
    end,
    {duration, Value} = Duration,
    {Unit_counts, Remaining} = extract(Value, Unit_value),
    case Remaining >= ((Unit_value * 2) div 3) of
        true ->
            Unit_counts + 1;

        false ->
            Unit_counts
    end.

-spec inner_blur(list({integer(), unit()})) -> {integer(), unit()}.
inner_blur(Values) ->
    [Second, Leading | _] = case Values of
        [_, _ | _] -> Values;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl/duration"/utf8>>,
                        function => <<"inner_blur"/utf8>>,
                        line => 254})
    end,
    _assert_subject = gleam@list:key_find(
        [{year, 31536000000000},
            {month, 2592000000000},
            {week, 604800000000},
            {day, 86400000000},
            {hour, 3600000000},
            {minute, 60000000},
            {second, 1000000},
            {milli_second, 1000},
            {micro_second, 1}],
        erlang:element(2, Leading)
    ),
    {ok, Leading_unit} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"birl/duration"/utf8>>,
                        function => <<"inner_blur"/utf8>>,
                        line => 255})
    end,
    _assert_subject@1 = gleam@list:key_find(
        [{year, 31536000000000},
            {month, 2592000000000},
            {week, 604800000000},
            {day, 86400000000},
            {hour, 3600000000},
            {minute, 60000000},
            {second, 1000000},
            {milli_second, 1000},
            {micro_second, 1}],
        erlang:element(2, Second)
    ),
    {ok, Second_unit} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"birl/duration"/utf8>>,
                        function => <<"inner_blur"/utf8>>,
                        line => 256})
    end,
    Leading@1 = case (erlang:element(1, Second) * Second_unit) < ((Leading_unit
    * 2)
    div 3) of
        true ->
            Leading;

        false ->
            {erlang:element(1, Leading) + 1, erlang:element(2, Leading)}
    end,
    case gleam@list:drop(Values, 2) of
        [] ->
            Leading@1;

        Chopped ->
            inner_blur([Leading@1 | Chopped])
    end.

-spec blur(duration()) -> {integer(), unit()}.
blur(Duration) ->
    case decompose(Duration) of
        [] ->
            {0, micro_second};

        Decomposed ->
            _pipe = Decomposed,
            _pipe@1 = lists:reverse(_pipe),
            inner_blur(_pipe@1)
    end.

-spec parse(binary()) -> {ok, duration()} | {error, nil}.
parse(Expression) ->
    _assert_subject = gleam@regex:from_string(
        <<"([+|\\-])?\\s*(\\d+)\\s*(\\w+)?"/utf8>>
    ),
    {ok, Re} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"birl/duration"/utf8>>,
                        function => <<"parse"/utf8>>,
                        line => 319})
    end,
    {Constructor, Expression@2} = case gleam@string:starts_with(
        Expression,
        <<"accurate:"/utf8>>
    ) of
        true ->
            _assert_subject@1 = gleam@string:split(Expression, <<":"/utf8>>),
            [_, Expression@1] = case _assert_subject@1 of
                [_, _] -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"birl/duration"/utf8>>,
                                function => <<"parse"/utf8>>,
                                line => 325})
            end,
            {fun accurate_new/1, Expression@1};

        false ->
            {fun new/1, Expression}
    end,
    case begin
        _pipe = Expression@2,
        _pipe@1 = gleam@string:lowercase(_pipe),
        _pipe@2 = gleam@regex:scan(Re, _pipe@1),
        gleam@list:try_map(_pipe@2, fun(Item) -> case Item of
                    {match, _, [Sign_option, {some, Amount_string}]} ->
                        gleam@result:then(
                            gleam@int:parse(Amount_string),
                            fun(Amount) -> case Sign_option of
                                    {some, <<"-"/utf8>>} ->
                                        {ok, {-1 * Amount, micro_second}};

                                    none ->
                                        {ok, {Amount, micro_second}};

                                    {some, <<"+"/utf8>>} ->
                                        {ok, {Amount, micro_second}};

                                    {some, _} ->
                                        {error, nil}
                                end end
                        );

                    {match,
                        _,
                        [Sign_option@1, {some, Amount_string@1}, {some, Unit}]} ->
                        gleam@result:then(
                            gleam@int:parse(Amount_string@1),
                            fun(Amount@1) ->
                                gleam@result:then(
                                    gleam@list:find(
                                        [{year,
                                                [<<"y"/utf8>>,
                                                    <<"year"/utf8>>,
                                                    <<"years"/utf8>>]},
                                            {month,
                                                [<<"mon"/utf8>>,
                                                    <<"month"/utf8>>,
                                                    <<"months"/utf8>>]},
                                            {week,
                                                [<<"w"/utf8>>,
                                                    <<"week"/utf8>>,
                                                    <<"weeks"/utf8>>]},
                                            {day,
                                                [<<"d"/utf8>>,
                                                    <<"day"/utf8>>,
                                                    <<"days"/utf8>>]},
                                            {hour,
                                                [<<"h"/utf8>>,
                                                    <<"hour"/utf8>>,
                                                    <<"hours"/utf8>>]},
                                            {minute,
                                                [<<"m"/utf8>>,
                                                    <<"min"/utf8>>,
                                                    <<"minute"/utf8>>,
                                                    <<"minutes"/utf8>>]},
                                            {second,
                                                [<<"s"/utf8>>,
                                                    <<"sec"/utf8>>,
                                                    <<"secs"/utf8>>,
                                                    <<"second"/utf8>>,
                                                    <<"seconds"/utf8>>]},
                                            {milli_second,
                                                [<<"ms"/utf8>>,
                                                    <<"msec"/utf8>>,
                                                    <<"msecs"/utf8>>,
                                                    <<"millisecond"/utf8>>,
                                                    <<"milliseconds"/utf8>>,
                                                    <<"milli-second"/utf8>>,
                                                    <<"milli-seconds"/utf8>>,
                                                    <<"milli_second"/utf8>>,
                                                    <<"milli_seconds"/utf8>>]}],
                                        fun(Item@1) ->
                                            gleam@list:contains(
                                                erlang:element(2, Item@1),
                                                Unit
                                            )
                                        end
                                    ),
                                    fun(_use0) ->
                                        {Unit@1, _} = _use0,
                                        case Sign_option@1 of
                                            {some, <<"-"/utf8>>} ->
                                                {ok, {-1 * Amount@1, Unit@1}};

                                            none ->
                                                {ok, {Amount@1, Unit@1}};

                                            {some, <<"+"/utf8>>} ->
                                                {ok, {Amount@1, Unit@1}};

                                            {some, _} ->
                                                {error, nil}
                                        end
                                    end
                                )
                            end
                        );

                    _ ->
                        {error, nil}
                end end)
    end of
        {ok, Values} ->
            _pipe@3 = Values,
            _pipe@4 = Constructor(_pipe@3),
            {ok, _pipe@4};

        {error, nil} ->
            {error, nil}
    end.
