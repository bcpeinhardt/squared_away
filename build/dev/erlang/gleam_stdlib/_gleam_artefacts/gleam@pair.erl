-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({AAP, any()}) -> AAP.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), AAS}) -> AAS.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({AAT, AAU}) -> {AAU, AAT}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({AAV, AAW}, fun((AAV) -> AAX)) -> {AAX, AAW}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({AAY, AAZ}, fun((AAZ) -> ABA)) -> {AAY, ABA}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(ABB, ABC) -> {ABB, ABC}.
new(First, Second) ->
    {First, Second}.
