-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FEA) :: {set, gleam@dict:dict(FEA, list(nil))}.

-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-spec contains(set(FEL), FEL) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-spec delete(set(FEN), FEN) -> set(FEN).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-spec to_list(set(FEQ)) -> list(FEQ).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-spec fold(set(FEW), FEY, fun((FEY, FEW) -> FEY)) -> FEY.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-spec filter(set(FEZ), fun((FEZ) -> boolean())) -> set(FEZ).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-spec drop(set(FFG), list(FFG)) -> set(FFG).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-spec take(set(FFK), list(FFK)) -> set(FFK).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-spec order(set(FFO), set(FFO)) -> {set(FFO), set(FFO)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-spec intersection(set(FFX), set(FFX)) -> set(FFX).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-spec difference(set(FGB), set(FGB)) -> set(FGB).
difference(First, Second) ->
    drop(First, to_list(Second)).

-spec is_subset(set(FGF), set(FGF)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-spec is_disjoint(set(FGI), set(FGI)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-spec insert(set(FEI), FEI) -> set(FEI).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-spec from_list(list(FET)) -> set(FET).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-spec map(set(FFC), fun((FFC) -> FFE)) -> set(FFE).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-spec union(set(FFT), set(FFT)) -> set(FFT).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-spec symmetric_difference(set(FGL), set(FGL)) -> set(FGL).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).
