-module(repeatedly).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([call/3, stop/1, set_function/2, update_state/2, set_state/2]).
-export_type([repeater/1]).

-type repeater(YNY) :: any() | {gleam_phantom, YNY}.

-spec call(integer(), YNZ, fun((YNZ, integer()) -> any())) -> repeater(YNZ).
call(Delay_ms, State, Function) ->
    repeatedly_ffi:call(Delay_ms, State, Function).

-spec stop(repeater(any())) -> nil.
stop(Repeater) ->
    repeatedly_ffi:stop(Repeater).

-spec set_function(repeater(YOE), fun((YOE, integer()) -> any())) -> nil.
set_function(Repeater, Function) ->
    repeatedly_ffi:replace(Repeater, Function).

-spec update_state(repeater(YOJ), fun((YOJ) -> YOJ)) -> nil.
update_state(Repeater, Function) ->
    repeatedly_ffi:update_state(Repeater, Function).

-spec set_state(repeater(YOH), YOH) -> nil.
set_state(Repeater, State) ->
    repeatedly_ffi:update_state(Repeater, fun(_) -> State end).
