-module(envoy).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/1, set/2, unset/1, all/0]).

-spec get(binary()) -> {ok, binary()} | {error, nil}.
get(Name) ->
    envoy_ffi:get(Name).

-spec set(binary(), binary()) -> nil.
set(Name, Value) ->
    envoy_ffi:set(Name, Value).

-spec unset(binary()) -> nil.
unset(Name) ->
    envoy_ffi:unset(Name).

-spec all() -> gleam@dict:dict(binary(), binary()).
all() ->
    envoy_ffi:all().
