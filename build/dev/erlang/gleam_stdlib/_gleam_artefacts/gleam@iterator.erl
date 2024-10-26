-module(gleam@iterator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, transform/3, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, map2/3, append/2, flatten/1, concat/1, flat_map/2, filter/2, filter_map/2, cycle/1, find/2, find_map/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, range/2, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2, length/1, each/2, yield/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(BXV) :: stop | {continue, BXV, fun(() -> action(BXV))}.

-opaque iterator(BXW) :: {iterator, fun(() -> action(BXW))}.

-type step(BXX, BXY) :: {next, BXX, BXY} | done.

-type chunk(BXZ, BYA) :: {another_by,
        list(BXZ),
        BYA,
        BXZ,
        fun(() -> action(BXZ))} |
    {last_by, list(BXZ)}.

-type sized_chunk(BYB) :: {another, list(BYB), fun(() -> action(BYB))} |
    {last, list(BYB)} |
    no_more.

-spec stop() -> action(any()).
stop() ->
    stop.

-spec do_unfold(BYE, fun((BYE) -> step(BYF, BYE))) -> fun(() -> action(BYF)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-spec unfold(BYJ, fun((BYJ) -> step(BYK, BYJ))) -> iterator(BYK).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-spec repeatedly(fun(() -> BYO)) -> iterator(BYO).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-spec repeat(BYQ) -> iterator(BYQ).
repeat(X) ->
    repeatedly(fun() -> X end).

-spec from_list(list(BYS)) -> iterator(BYS).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-spec do_transform(
    fun(() -> action(BYV)),
    BYX,
    fun((BYX, BYV) -> step(BYY, BYX))
) -> fun(() -> action(BYY)).
do_transform(Continuation, State, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                case F(State, El) of
                    done ->
                        stop;

                    {next, Yield, Next_state} ->
                        {continue, Yield, do_transform(Next, Next_state, F)}
                end
        end end.

-spec transform(iterator(BZC), BZE, fun((BZE, BZC) -> step(BZF, BZE))) -> iterator(BZF).
transform(Iterator, Initial, F) ->
    _pipe = do_transform(erlang:element(2, Iterator), Initial, F),
    {iterator, _pipe}.

-spec do_fold(fun(() -> action(BZJ)), fun((BZL, BZJ) -> BZL), BZL) -> BZL.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-spec fold(iterator(BZM), BZO, fun((BZO, BZM) -> BZO)) -> BZO.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-spec to_list(iterator(BZR)) -> list(BZR).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    lists:reverse(_pipe@1).

-spec step(iterator(BZU)) -> step(BZU, iterator(BZU)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-spec do_take(fun(() -> action(BZZ)), integer()) -> fun(() -> action(BZZ)).
do_take(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, do_take(Next, Desired - 1)}
                end
        end end.

-spec take(iterator(CAC), integer()) -> iterator(CAC).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-spec do_drop(fun(() -> action(CAF)), integer()) -> action(CAF).
do_drop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    do_drop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-spec drop(iterator(CAI), integer()) -> iterator(CAI).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-spec do_map(fun(() -> action(CAL)), fun((CAL) -> CAN)) -> fun(() -> action(CAN)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-spec map(iterator(CAP), fun((CAP) -> CAR)) -> iterator(CAR).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-spec do_map2(
    fun(() -> action(CAT)),
    fun(() -> action(CAV)),
    fun((CAT, CAV) -> CAX)
) -> fun(() -> action(CAX)).
do_map2(Continuation1, Continuation2, Fun) ->
    fun() -> case Continuation1() of
            stop ->
                stop;

            {continue, A, Next_a} ->
                case Continuation2() of
                    stop ->
                        stop;

                    {continue, B, Next_b} ->
                        {continue, Fun(A, B), do_map2(Next_a, Next_b, Fun)}
                end
        end end.

-spec map2(iterator(CAZ), iterator(CBB), fun((CAZ, CBB) -> CBD)) -> iterator(CBD).
map2(Iterator1, Iterator2, Fun) ->
    _pipe = do_map2(
        erlang:element(2, Iterator1),
        erlang:element(2, Iterator2),
        Fun
    ),
    {iterator, _pipe}.

-spec do_append(fun(() -> action(CBF)), fun(() -> action(CBF))) -> action(CBF).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-spec append(iterator(CBJ), iterator(CBJ)) -> iterator(CBJ).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-spec do_flatten(fun(() -> action(iterator(CBN)))) -> action(CBN).
do_flatten(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            do_append(
                erlang:element(2, It),
                fun() -> do_flatten(Next_iterator) end
            )
    end.

-spec flatten(iterator(iterator(CBR))) -> iterator(CBR).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-spec concat(list(iterator(CBV))) -> iterator(CBV).
concat(Iterators) ->
    flatten(from_list(Iterators)).

-spec flat_map(iterator(CBZ), fun((CBZ) -> iterator(CCB))) -> iterator(CCB).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-spec do_filter(fun(() -> action(CCE)), fun((CCE) -> boolean())) -> action(CCE).
do_filter(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> do_filter(Iterator, Predicate) end};

                false ->
                    do_filter(Iterator, Predicate)
            end
    end.

-spec filter(iterator(CCH), fun((CCH) -> boolean())) -> iterator(CCH).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-spec do_filter_map(
    fun(() -> action(CCK)),
    fun((CCK) -> {ok, CCM} | {error, any()})
) -> action(CCM).
do_filter_map(Continuation, F) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {continue, E@1, fun() -> do_filter_map(Next, F) end};

                {error, _} ->
                    do_filter_map(Next, F)
            end
    end.

-spec filter_map(iterator(CCR), fun((CCR) -> {ok, CCT} | {error, any()})) -> iterator(CCT).
filter_map(Iterator, F) ->
    _pipe = fun() -> do_filter_map(erlang:element(2, Iterator), F) end,
    {iterator, _pipe}.

-spec cycle(iterator(CCY)) -> iterator(CCY).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-spec do_find(fun(() -> action(CDC)), fun((CDC) -> boolean())) -> {ok, CDC} |
    {error, nil}.
do_find(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    do_find(Next, F)
            end
    end.

-spec find(iterator(CDG), fun((CDG) -> boolean())) -> {ok, CDG} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-spec do_find_map(
    fun(() -> action(CDK)),
    fun((CDK) -> {ok, CDM} | {error, any()})
) -> {ok, CDM} | {error, nil}.
do_find_map(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                {ok, E@1} ->
                    {ok, E@1};

                {error, _} ->
                    do_find_map(Next, F)
            end
    end.

-spec find_map(iterator(CDS), fun((CDS) -> {ok, CDU} | {error, any()})) -> {ok,
        CDU} |
    {error, nil}.
find_map(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find_map(_pipe, Is_desired).

-spec do_index(fun(() -> action(CEA)), integer()) -> fun(() -> action({CEA,
    integer()})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {E, Next}, do_index(Continuation@1, Next + 1)}
        end end.

-spec index(iterator(CED)) -> iterator({CED, integer()}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-spec iterate(CEG, fun((CEG) -> CEG)) -> iterator(CEG).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-spec do_take_while(fun(() -> action(CEI)), fun((CEI) -> boolean())) -> fun(() -> action(CEI)).
do_take_while(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, do_take_while(Next, Predicate)}
                end
        end end.

-spec take_while(iterator(CEL), fun((CEL) -> boolean())) -> iterator(CEL).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-spec do_drop_while(fun(() -> action(CEO)), fun((CEO) -> boolean())) -> action(CEO).
do_drop_while(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    do_drop_while(Next, Predicate)
            end
    end.

-spec drop_while(iterator(CER), fun((CER) -> boolean())) -> iterator(CER).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-spec do_scan(fun(() -> action(CEU)), fun((CEW, CEU) -> CEW), CEW) -> fun(() -> action(CEW)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-spec scan(iterator(CEY), CFA, fun((CFA, CEY) -> CFA)) -> iterator(CFA).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-spec do_zip(fun(() -> action(CFC)), fun(() -> action(CFE))) -> fun(() -> action({CFC,
    CFE})).
do_zip(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                            {El_left, El_right},
                            do_zip(Next_left, Next_right)}
                end
        end end.

-spec zip(iterator(CFH), iterator(CFJ)) -> iterator({CFH, CFJ}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-spec next_chunk(fun(() -> action(CFM)), fun((CFM) -> CFO), CFO, list(CFM)) -> chunk(CFM, CFO).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, lists:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by, lists:reverse(Current_chunk), Key, E, Next}
            end
    end.

-spec do_chunk(fun(() -> action(CFS)), fun((CFS) -> CFU), CFU, CFS) -> action(list(CFS)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-spec chunk(iterator(CFX), fun((CFX) -> any())) -> iterator(list(CFX)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-spec next_sized_chunk(fun(() -> action(CGC)), integer(), list(CGC)) -> sized_chunk(CGC).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, lists:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, lists:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-spec do_sized_chunk(fun(() -> action(CGG)), integer()) -> fun(() -> action(list(CGG))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-spec sized_chunk(iterator(CGK), integer()) -> iterator(list(CGK)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-spec do_intersperse(fun(() -> action(CGO)), CGO) -> action(CGO).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-spec intersperse(iterator(CGR), CGR) -> iterator(CGR).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-spec do_any(fun(() -> action(CGU)), fun((CGU) -> boolean())) -> boolean().
do_any(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    true;

                false ->
                    do_any(Next, Predicate)
            end
    end.

-spec any(iterator(CGW), fun((CGW) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-spec do_all(fun(() -> action(CGY)), fun((CGY) -> boolean())) -> boolean().
do_all(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            case Predicate(E) of
                true ->
                    do_all(Next, Predicate);

                false ->
                    false
            end
    end.

-spec all(iterator(CHA), fun((CHA) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-spec update_group_with(CHC) -> fun((gleam@option:option(list(CHC))) -> list(CHC)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-spec group_updater(fun((CHG) -> CHH)) -> fun((gleam@dict:dict(CHH, list(CHG)), CHG) -> gleam@dict:dict(CHH, list(CHG))).
group_updater(F) ->
    fun(Groups, Elem) -> _pipe = Groups,
        gleam@dict:upsert(_pipe, F(Elem), update_group_with(Elem)) end.

-spec group(iterator(CHO), fun((CHO) -> CHQ)) -> gleam@dict:dict(CHQ, list(CHO)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@dict:new(), group_updater(Key)),
    gleam@dict:map_values(_pipe@1, fun(_, Group) -> lists:reverse(Group) end).

-spec reduce(iterator(CHU), fun((CHU, CHU) -> CHU)) -> {ok, CHU} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-spec last(iterator(CHY)) -> {ok, CHY} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-spec once(fun(() -> CIE)) -> iterator(CIE).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-spec single(CIG) -> iterator(CIG).
single(Elem) ->
    once(fun() -> Elem end).

-spec do_interleave(fun(() -> action(CII)), fun(() -> action(CII))) -> action(CII).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-spec interleave(iterator(CIM), iterator(CIM)) -> iterator(CIM).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-spec do_fold_until(
    fun(() -> action(CIQ)),
    fun((CIS, CIQ) -> gleam@list:continue_or_stop(CIS)),
    CIS
) -> CIS.
do_fold_until(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    do_fold_until(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-spec fold_until(
    iterator(CIU),
    CIW,
    fun((CIW, CIU) -> gleam@list:continue_or_stop(CIW))
) -> CIW.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-spec do_try_fold(
    fun(() -> action(CIY)),
    fun((CJA, CIY) -> {ok, CJA} | {error, CJB}),
    CJA
) -> {ok, CJA} | {error, CJB}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            gleam@result:'try'(
                F(Accumulator, Elem),
                fun(Accumulator@1) -> do_try_fold(Next, F, Accumulator@1) end
            )
    end.

-spec try_fold(iterator(CJG), CJI, fun((CJI, CJG) -> {ok, CJI} | {error, CJJ})) -> {ok,
        CJI} |
    {error, CJJ}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-spec first(iterator(CJO)) -> {ok, CJO} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _} ->
            {ok, E}
    end.

-spec at(iterator(CJS), integer()) -> {ok, CJS} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-spec do_length(fun(() -> action(any())), integer()) -> integer().
do_length(Continuation, Length) ->
    case Continuation() of
        stop ->
            Length;

        {continue, _, Next} ->
            do_length(Next, Length + 1)
    end.

-spec length(iterator(any())) -> integer().
length(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    do_length(_pipe, 0).

-spec each(iterator(CKA), fun((CKA) -> any())) -> nil.
each(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    run(_pipe@1).

-spec yield(CKD, fun(() -> iterator(CKD))) -> iterator(CKD).
yield(Element, Next) ->
    {iterator,
        fun() ->
            {continue, Element, fun() -> (erlang:element(2, Next()))() end}
        end}.
