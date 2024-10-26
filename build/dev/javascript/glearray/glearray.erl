-module(glearray).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, from_list/1, to_list/1, length/1, get/2, copy_set/3, copy_push/2, copy_insert/3, iterate/1]).
-export_type([array/1]).

-type array(FLP) :: any() | {gleam_phantom, FLP}.

-spec new() -> array(any()).
new() ->
    glearray_ffi:new().

-spec from_list(list(FLS)) -> array(FLS).
from_list(List) ->
    erlang:list_to_tuple(List).

-spec to_list(array(FLV)) -> list(FLV).
to_list(Array) ->
    erlang:tuple_to_list(Array).

-spec length(array(any())) -> integer().
length(Array) ->
    erlang:tuple_size(Array).

-spec is_valid_index(array(any()), integer()) -> boolean().
is_valid_index(Array, Index) ->
    (Index >= 0) andalso (Index < erlang:tuple_size(Array)).

-spec get(array(FMA), integer()) -> {ok, FMA} | {error, nil}.
get(Array, Index) ->
    case is_valid_index(Array, Index) of
        true ->
            {ok, glearray_ffi:get(Array, Index)};

        false ->
            {error, nil}
    end.

-spec copy_set(array(FMG), integer(), FMG) -> {ok, array(FMG)} | {error, nil}.
copy_set(Array, Index, Value) ->
    case is_valid_index(Array, Index) of
        true ->
            {ok, glearray_ffi:set(Array, Index, Value)};

        false ->
            {error, nil}
    end.

-spec copy_push(array(FMQ), FMQ) -> array(FMQ).
copy_push(Array, Value) ->
    erlang:append_element(Array, Value).

-spec copy_insert(array(FMT), integer(), FMT) -> {ok, array(FMT)} | {error, nil}.
copy_insert(Array, Index, Value) ->
    case (Index >= 0) andalso (Index =< erlang:tuple_size(Array)) of
        true ->
            {ok, glearray_ffi:insert(Array, Index, Value)};

        false ->
            {error, nil}
    end.

-spec iterate(array(FNB)) -> gleam@iterator:iterator(FNB).
iterate(Array) ->
    gleam@iterator:unfold(0, fun(Index) -> case get(Array, Index) of
                {ok, Element} ->
                    {next, Element, Index + 1};

                {error, _} ->
                    done
            end end).
