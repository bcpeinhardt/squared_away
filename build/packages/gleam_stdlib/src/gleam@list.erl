-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map2/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, concat/1, flatten/1, flat_map/2, fold/3, count/2, group/2, map_fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([continue_or_stop/1, sorting/0]).

-type continue_or_stop(AAS) :: {continue, AAS} | {stop, AAS}.

-type sorting() :: ascending | descending.

-spec count_length(list(any()), integer()) -> integer().
count_length(List, Count) ->
    case List of
        [_ | List@1] ->
            count_length(List@1, Count + 1);

        _ ->
            Count
    end.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec do_reverse(list(ASP), list(ASP)) -> list(ASP).
do_reverse(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            do_reverse(Rest, [Item | Accumulator])
    end.

-spec reverse(list(AAZ)) -> list(AAZ).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(ABH), ABH) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-spec first(list(ABJ)) -> {ok, ABJ} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-spec rest(list(ABN)) -> {ok, list(ABN)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-spec update_group(fun((ABS) -> ABT)) -> fun((gleam@dict:dict(ABT, list(ABS)), ABS) -> gleam@dict:dict(ABT, list(ABS))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@dict:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@dict:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@dict:insert(Groups, F(Elem), [Elem])
        end end.

-spec do_filter(list(ACG), fun((ACG) -> boolean()), list(ACG)) -> list(ACG).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(ACK), fun((ACK) -> boolean())) -> list(ACK).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(
    list(ACN),
    fun((ACN) -> {ok, ACP} | {error, any()}),
    list(ACP)
) -> list(ACP).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(ACV), fun((ACV) -> {ok, ACX} | {error, any()})) -> list(ACX).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(ADC), fun((ADC) -> ADE), list(ADE)) -> list(ADE).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(ADH), fun((ADH) -> ADJ)) -> list(ADJ).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec do_map2(list(ADR), list(ADT), fun((ADR, ADT) -> ADV), list(ADV)) -> list(ADV).
do_map2(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            do_map2(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-spec map2(list(ADL), list(ADN), fun((ADL, ADN) -> ADP)) -> list(ADP).
map2(List1, List2, Fun) ->
    do_map2(List1, List2, Fun, []).

-spec do_index_map(
    list(AED),
    fun((AED, integer()) -> AEF),
    integer(),
    list(AEF)
) -> list(AEF).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(X, Index) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(AEI), fun((AEI, integer()) -> AEK)) -> list(AEK).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(AEM), fun((AEM) -> {ok, AEO} | {error, AEP}), list(AEO)) -> {ok,
        list(AEO)} |
    {error, AEP}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(AEW), fun((AEW) -> {ok, AEY} | {error, AEZ})) -> {ok,
        list(AEY)} |
    {error, AEZ}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(AFF), integer()) -> list(AFF).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(AFI), integer(), list(AFI)) -> list(AFI).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(AFM), integer()) -> list(AFM).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec wrap(AFR) -> list(AFR).
wrap(Item) ->
    [Item].

-spec do_append(list(AFX), list(AFX)) -> list(AFX).
do_append(First, Second) ->
    case First of
        [] ->
            Second;

        [Item | Rest] ->
            do_append(Rest, [Item | Second])
    end.

-spec append(list(AFT), list(AFT)) -> list(AFT).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(AGB), AGB) -> list(AGB).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(AGE), list(AGE)) -> list(AGE).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [First | Rest] ->
            reverse_and_prepend(Rest, [First | Suffix])
    end.

-spec do_concat(list(list(AGI)), list(AGI)) -> list(AGI).
do_concat(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            do_concat(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec concat(list(list(AGN))) -> list(AGN).
concat(Lists) ->
    do_concat(Lists, []).

-spec flatten(list(list(AGR))) -> list(AGR).
flatten(Lists) ->
    do_concat(Lists, []).

-spec flat_map(list(AGV), fun((AGV) -> list(AGX))) -> list(AGX).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    concat(_pipe).

-spec fold(list(AHA), AHC, fun((AHC, AHA) -> AHC)) -> AHC.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec count(list(AAX), fun((AAX) -> boolean())) -> integer().
count(List, Predicate) ->
    fold(List, 0, fun(Acc, Value) -> case Predicate(Value) of
                true ->
                    Acc + 1;

                false ->
                    Acc
            end end).

-spec group(list(ACA), fun((ACA) -> ACC)) -> gleam@dict:dict(ACC, list(ACA)).
group(List, Key) ->
    fold(List, gleam@dict:new(), update_group(Key)).

-spec map_fold(list(ADY), AEA, fun((AEA, ADY) -> {AEA, AEB})) -> {AEA,
    list(AEB)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun lists:reverse/1).

-spec fold_right(list(AHD), AHF, fun((AHF, AHD) -> AHF)) -> AHF.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(
    list(AHG),
    AHI,
    fun((AHI, AHG, integer()) -> AHI),
    integer()
) -> AHI.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(AHJ), AHL, fun((AHL, AHJ, integer()) -> AHL)) -> AHL.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(AHM), AHO, fun((AHO, AHM) -> {ok, AHO} | {error, AHP})) -> {ok,
        AHO} |
    {error, AHP}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-spec fold_until(list(AHU), AHW, fun((AHW, AHU) -> continue_or_stop(AHW))) -> AHW.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(AHY), fun((AHY) -> boolean())) -> {ok, AHY} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(AIC), fun((AIC) -> {ok, AIE} | {error, any()})) -> {ok, AIE} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(AIK), fun((AIK) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(AIM), fun((AIM) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-spec do_zip(list(AIO), list(AIQ), list({AIO, AIQ})) -> list({AIO, AIQ}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-spec zip(list(AIU), list(AIW)) -> list({AIU, AIW}).
zip(List, Other) ->
    do_zip(List, Other, []).

-spec strict_zip(list(AIZ), list(AJB)) -> {ok, list({AIZ, AJB})} | {error, nil}.
strict_zip(List, Other) ->
    case erlang:length(List) =:= erlang:length(Other) of
        true ->
            {ok, zip(List, Other)};

        false ->
            {error, nil}
    end.

-spec do_unzip(list({AZU, AZV}), list(AZU), list(AZV)) -> {list(AZU), list(AZV)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {lists:reverse(Xs), lists:reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({AJK, AJL})) -> {list(AJK), list(AJL)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(AJP), AJP, list(AJP)) -> list(AJP).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(AJT), AJT) -> list(AJT).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec unique(list(AJW)) -> list(AJW).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec sequences(
    list(AKC),
    fun((AKC, AKC) -> gleam@order:order()),
    list(AKC),
    sorting(),
    AKC,
    list(list(AKC))
) -> list(list(AKC)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [do_reverse(Growing@1, []) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [do_reverse(Growing@1, []) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-spec merge_ascendings(
    list(AKZ),
    list(AKZ),
    fun((AKZ, AKZ) -> gleam@order:order()),
    list(AKZ)
) -> list(AKZ).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-spec merge_ascending_pairs(
    list(list(AKN)),
    fun((AKN, AKN) -> gleam@order:order()),
    list(list(AKN))
) -> list(list(AKN)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-spec merge_descendings(
    list(ALE),
    list(ALE),
    fun((ALE, ALE) -> gleam@order:order()),
    list(ALE)
) -> list(ALE).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            do_reverse(List, Acc);

        {List, []} ->
            do_reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-spec merge_descending_pairs(
    list(list(AKT)),
    fun((AKT, AKT) -> gleam@order:order()),
    list(list(AKT))
) -> list(list(AKT)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            do_reverse(Acc, []);

        [Sequence] ->
            do_reverse([do_reverse(Sequence, []) | Acc], []);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-spec merge_all(
    list(list(AKJ)),
    sorting(),
    fun((AKJ, AKJ) -> gleam@order:order())
) -> list(AKJ).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            do_reverse(Sequence@1, []);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-spec sort(list(AJZ), fun((AJZ, AJZ) -> gleam@order:order())) -> list(AJZ).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            tail_recursive_range(Start, Stop + 1, [Stop | Acc]);

        lt ->
            tail_recursive_range(Start, Stop - 1, [Stop | Acc])
    end.

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec do_repeat(ALM, integer(), list(ALM)) -> list(ALM).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(ALP, integer()) -> list(ALP).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(ALR), integer(), list(ALR)) -> {list(ALR), list(ALR)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(ALW), integer()) -> {list(ALW), list(ALW)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(AMA), fun((AMA) -> boolean()), list(AMA)) -> {list(AMA),
    list(AMA)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(AMF), fun((AMF) -> boolean())) -> {list(AMF), list(AMF)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({AMJ, AMK}), AMJ) -> {ok, AMK} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec key_filter(list({AMO, AMP}), AMO) -> list(AMP).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(BEV), fun((BEV) -> boolean()), list(BEV)) -> {ok,
        {BEV, list(BEV)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(AMW), fun((AMW) -> boolean())) -> {ok, {AMW, list(AMW)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(BFJ), fun((BFJ) -> {ok, BFW} | {error, any()}), list(BFJ)) -> {ok,
        {BFW, list(BFJ)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(ANF), fun((ANF) -> {ok, ANH} | {error, any()})) -> {ok,
        {ANH, list(ANF)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({ANO, ANP}), ANO) -> {ok, {ANP, list({ANO, ANP})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({ANU, ANV}), ANU, ANV) -> list({ANU, ANV}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(ANY), fun((ANY) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec try_each(list(AOB), fun((AOB) -> {ok, any()} | {error, AOE})) -> {ok, nil} |
    {error, AOE}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-spec do_partition(list(BHD), fun((BHD) -> boolean()), list(BHD), list(BHD)) -> {list(BHD),
    list(BHD)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(AOO), fun((AOO) -> boolean())) -> {list(AOO), list(AOO)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(AOS)) -> list(list(AOS)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I, I_idx) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = lists:reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            concat(_pipe@5)
    end.

-spec do_window(list(list(AOW)), list(AOW), integer()) -> list(list(AOW)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case erlang:length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(APC), integer()) -> list(list(APC)).
window(L, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            _pipe = do_window([], L, N),
            lists:reverse(_pipe)
    end.

-spec window_by_2(list(APG)) -> list({APG, APG}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(APJ), fun((APJ) -> boolean())) -> list(APJ).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(APM), fun((APM) -> boolean()), list(APM)) -> list(APM).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    do_take_while(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-spec take_while(list(APQ), fun((APQ) -> boolean())) -> list(APQ).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(APT), fun((APT) -> APV), APV, list(APT), list(list(APT))) -> list(list(APT)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    do_chunk(Rest, F, Key, [First], New_acc);

                _ ->
                    do_chunk(Rest, F, Key, [First | Current_chunk], Acc)
            end;

        _ ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(AQB), fun((AQB) -> any())) -> list(list(AQB)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            do_chunk(Rest, F, F(First), [First], [])
    end.

-spec do_sized_chunk(
    list(AQG),
    integer(),
    integer(),
    list(AQG),
    list(list(AQG))
) -> list(list(AQG)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Rest, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(AQN), integer()) -> list(list(AQN)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(AQR), fun((AQR, AQR) -> AQR)) -> {ok, AQR} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-spec do_scan(list(AQV), AQX, list(AQX), fun((AQX, AQV) -> AQX)) -> list(AQX).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(ARA), ARC, fun((ARC, ARA) -> ARC)) -> list(ARC).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(ARE)) -> {ok, ARE} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(ARI), integer()) -> list(list(ARI)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        lists:reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(ARM)) -> list(list({ARM, ARM})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(ARQ)) -> list({ARQ, ARQ}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    concat(_pipe).

-spec transpose(list(list(ARX))) -> list(list(ARX)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                concat(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec interleave(list(list(ART))) -> list(ART).
interleave(List) ->
    _pipe = transpose(List),
    concat(_pipe).

-spec do_shuffle_pair_unwrap(list({float(), ASC}), list(ASC)) -> list(ASC).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec do_shuffle_by_pair_indexes(list({float(), ASG})) -> list({float(), ASG}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec shuffle(list(ASJ)) -> list(ASJ).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
