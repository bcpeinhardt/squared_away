-module(mist@internal@buffer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([empty/0, new/1, append/2, slice/2, with_capacity/2, size/1]).
-export_type([buffer/0]).

-type buffer() :: {buffer, integer(), bitstring()}.

-spec empty() -> buffer().
empty() ->
    {buffer, 0, <<>>}.

-spec new(bitstring()) -> buffer().
new(Data) ->
    {buffer, 0, Data}.

-spec append(buffer(), bitstring()) -> buffer().
append(Buffer, Data) ->
    Data_size = erlang:byte_size(Data),
    Remaining = gleam@int:max(erlang:element(2, Buffer) - Data_size, 0),
    {buffer,
        Remaining,
        <<(erlang:element(3, Buffer))/bitstring, Data/bitstring>>}.

-spec slice(buffer(), integer()) -> {bitstring(), bitstring()}.
slice(Buffer, Bits) ->
    Bytes = Bits * 8,
    case erlang:element(3, Buffer) of
        <<Value:Bytes/bitstring, Rest/bitstring>> ->
            {Value, Rest};

        _ ->
            {erlang:element(3, Buffer), <<>>}
    end.

-spec with_capacity(buffer(), integer()) -> buffer().
with_capacity(Buffer, Size) ->
    erlang:setelement(2, Buffer, Size).

-spec size(integer()) -> buffer().
size(Remaining) ->
    {buffer, Remaining, <<>>}.
