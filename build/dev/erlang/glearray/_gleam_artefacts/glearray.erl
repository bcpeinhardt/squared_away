-module(glearray).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, length/1, get/2, copy_set/3, copy_push/2, copy_insert/3, iterate/1]).
-export_type([array/1]).

-type array(MCC) :: any() | {gleam_phantom, MCC}.

-spec new() -> array(any()).
new() ->
    glearray_ffi:new().

-spec from_list(list(MCF)) -> array(MCF).
from_list(List) ->
    erlang:list_to_tuple(List).

-spec to_list(array(MCI)) -> list(MCI).
to_list(Array) ->
    erlang:tuple_to_list(Array).

-spec length(array(any())) -> integer().
length(Array) ->
    erlang:tuple_size(Array).

-spec is_valid_index(array(any()), integer()) -> boolean().
is_valid_index(Array, Index) ->
    (Index >= 0) andalso (Index < erlang:tuple_size(Array)).

-spec get(array(MCN), integer()) -> {ok, MCN} | {error, nil}.
get(Array, Index) ->
    case is_valid_index(Array, Index) of
        true ->
            {ok, glearray_ffi:get(Array, Index)};

        false ->
            {error, nil}
    end.

-spec copy_set(array(MCT), integer(), MCT) -> {ok, array(MCT)} | {error, nil}.
copy_set(Array, Index, Value) ->
    case is_valid_index(Array, Index) of
        true ->
            {ok, glearray_ffi:set(Array, Index, Value)};

        false ->
            {error, nil}
    end.

-spec copy_push(array(MDD), MDD) -> array(MDD).
copy_push(Array, Value) ->
    erlang:append_element(Array, Value).

-spec copy_insert(array(MDG), integer(), MDG) -> {ok, array(MDG)} | {error, nil}.
copy_insert(Array, Index, Value) ->
    case (Index >= 0) andalso (Index =< erlang:tuple_size(Array)) of
        true ->
            {ok, glearray_ffi:insert(Array, Index, Value)};

        false ->
            {error, nil}
    end.

-spec iterate(array(MDO)) -> gleam@iterator:iterator(MDO).
iterate(Array) ->
    gleam@iterator:unfold(0, fun(Index) -> case get(Array, Index) of
                {ok, Element} ->
                    {next, Element, Index + 1};

                {error, _} ->
                    done
            end end).
