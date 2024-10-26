-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, is_empty/1, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, upsert/3, update/3, fold/3, map_values/2, filter/2, each/2, combine/3]).
-export_type([dict/2]).

-type dict(KS, KT) :: any() | {gleam_phantom, KS, KT}.

-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec to_list(dict(LC, LD)) -> list({LC, LD}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-spec is_empty(dict(any(), any())) -> boolean().
is_empty(Dict) ->
    Dict =:= new().

-spec get(dict(MJ, MK), MJ) -> {ok, MK} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec has_key(dict(LT, any()), LT) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-spec insert(dict(MV, MW), MV, MW) -> dict(MV, MW).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-spec fold_list_of_pair(list({LM, LN}), dict(LM, LN)) -> dict(LM, LN).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-spec from_list(list({LH, LI})) -> dict(LH, LI).
from_list(List) ->
    maps:from_list(List).

-spec reverse_and_concat(list(US), list(US)) -> list(US).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-spec do_keys_acc(list({OI, any()}), list(OI)) -> list(OI).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-spec keys(dict(NV, any())) -> list(NV).
keys(Dict) ->
    maps:keys(Dict).

-spec do_values_acc(list({any(), OY}), list(OY)) -> list(OY).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-spec values(dict(any(), OO)) -> list(OO).
values(Dict) ->
    maps:values(Dict).

-spec insert_taken(dict(QC, QD), list(QC), dict(QC, QD)) -> dict(QC, QD).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-spec take(dict(PO, PP), list(PO)) -> dict(PO, PP).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-spec insert_pair(dict(RB, RC), {RB, RC}) -> dict(RB, RC).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-spec fold_inserts(list({RH, RI}), dict(RH, RI)) -> dict(RH, RI).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-spec merge(dict(QL, QM), dict(QL, QM)) -> dict(QL, QM).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-spec delete(dict(RO, RP), RO) -> dict(RO, RP).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-spec drop(dict(SA, SB), list(SA)) -> dict(SA, SB).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-spec upsert(dict(SH, SI), SH, fun((gleam@option:option(SI)) -> SI)) -> dict(SH, SI).
upsert(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-spec update(dict(SO, SP), SO, fun((gleam@option:option(SP)) -> SP)) -> dict(SO, SP).
update(Dict, Key, Fun) ->
    upsert(Dict, Key, Fun).

-spec do_fold(list({SV, SW}), SY, fun((SY, SV, SW) -> SY)) -> SY.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(dict(SZ, TA), TD, fun((TD, SZ, TA) -> TD)) -> TD.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-spec map_values(dict(NH, NI), fun((NH, NI) -> NL)) -> dict(NH, NL).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-spec filter(dict(PC, PD), fun((PC, PD) -> boolean())) -> dict(PC, PD).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-spec each(dict(TE, TF), fun((TE, TF) -> any())) -> nil.
each(Dict, Fun) ->
    fold(
        Dict,
        nil,
        fun(Nil, K, V) ->
            Fun(K, V),
            Nil
        end
    ).

-spec combine(dict(TJ, TK), dict(TJ, TK), fun((TK, TK) -> TK)) -> dict(TJ, TK).
combine(Dict, Other, Fun) ->
    fold(Dict, Other, fun(Acc, Key, Value) -> case get(Acc, Key) of
                {ok, Other_value} ->
                    insert(Acc, Key, Fun(Value, Other_value));

                {error, _} ->
                    insert(Acc, Key, Value)
            end end).
