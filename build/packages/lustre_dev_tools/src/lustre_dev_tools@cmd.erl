-module(lustre_dev_tools@cmd).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([exec/3, cwd/0]).

-spec exec(binary(), list(binary()), binary()) -> {ok, binary()} |
    {error, {integer(), binary()}}.
exec(Command, Args, In) ->
    lustre_dev_tools_ffi:exec(Command, Args, In).

-spec cwd() -> {ok, binary()} | {error, gleam@dynamic:dynamic_()}.
cwd() ->
    lustre_dev_tools_ffi:get_cwd().
