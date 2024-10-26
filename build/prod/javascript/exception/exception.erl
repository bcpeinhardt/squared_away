-module(exception).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([rescue/1, defer/2]).
-export_type([exception/0]).

-type exception() :: {errored, gleam@dynamic:dynamic_()} |
    {thrown, gleam@dynamic:dynamic_()} |
    {exited, gleam@dynamic:dynamic_()}.

-spec rescue(fun(() -> FXC)) -> {ok, FXC} | {error, exception()}.
rescue(Body) ->
    exception_ffi:rescue(Body).

-spec defer(fun(() -> any()), fun(() -> FXG)) -> FXG.
defer(Cleanup, Body) ->
    exception_ffi:defer(Cleanup, Body).
