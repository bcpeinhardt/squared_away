-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({AAD, any()}) -> AAD.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), AAG}) -> AAG.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({AAH, AAI}) -> {AAI, AAH}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({AAJ, AAK}, fun((AAJ) -> AAL)) -> {AAL, AAK}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({AAM, AAN}, fun((AAN) -> AAO)) -> {AAM, AAO}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(AAP, AAQ) -> {AAP, AAQ}.
new(First, Second) ->
    {First, Second}.
