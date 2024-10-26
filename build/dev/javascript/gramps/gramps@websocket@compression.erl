-module(gramps@websocket@compression).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([init/0, inflate/2, deflate/2, set_controlling_process/2, close/1]).
-export_type([context/0, flush/0, deflated/0, default/0, compression/0]).

-type context() :: any().

-type flush() :: sync.

-type deflated() :: deflated.

-type default() :: default.

-type compression() :: {compression, context(), context()}.

-spec init() -> compression().
init() ->
    Inflate_context = zlib:open(),
    zlib:'inflateInit'(Inflate_context, -15),
    Deflate_context = zlib:open(),
    zlib:'deflateInit'(Deflate_context, default, deflated, -15, 8, default),
    {compression, Inflate_context, Deflate_context}.

-spec inflate(context(), bitstring()) -> bitstring().
inflate(Context, Data) ->
    _pipe = Context,
    _pipe@1 = zlib:inflate(
        _pipe,
        <<Data/bitstring, 16#00, 16#00, 16#FF, 16#FF>>
    ),
    erlang:list_to_bitstring(_pipe@1).

-spec deflate(context(), bitstring()) -> bitstring().
deflate(Context, Data) ->
    Data@1 = begin
        _pipe = Context,
        _pipe@1 = zlib:deflate(_pipe, Data, sync),
        erlang:list_to_bitstring(_pipe@1)
    end,
    Size = erlang:byte_size(Data@1) - 4,
    case Data@1 of
        <<Value:Size/binary, 16#00, 16#00, 16#FF, 16#FF>> ->
            Value;

        _ ->
            Data@1
    end.

-spec set_controlling_process(context(), gleam@erlang@process:pid_()) -> gleam@erlang@atom:atom_().
set_controlling_process(Context, Pid) ->
    zlib:set_controlling_process(Context, Pid).

-spec close(context()) -> nil.
close(Context) ->
    zlib:close(Context).
