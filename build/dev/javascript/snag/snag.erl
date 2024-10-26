-module(snag).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, error/1, layer/2, context/2, pretty_print/1, line_print/1]).
-export_type([snag/0]).

-type snag() :: {snag, binary(), list(binary())}.

-spec new(binary()) -> snag().
new(Issue) ->
    {snag, Issue, []}.

-spec error(binary()) -> {ok, any()} | {error, snag()}.
error(Issue) ->
    {error, new(Issue)}.

-spec layer(snag(), binary()) -> snag().
layer(Snag, Issue) ->
    {snag, Issue, [erlang:element(2, Snag) | erlang:element(3, Snag)]}.

-spec context({ok, FNQ} | {error, snag()}, binary()) -> {ok, FNQ} |
    {error, snag()}.
context(Result, Issue) ->
    case Result of
        {ok, _} ->
            Result;

        {error, Snag} ->
            {error, layer(Snag, Issue)}
    end.

-spec pretty_print_cause(list(binary())) -> gleam@string_builder:string_builder().
pretty_print_cause(Cause) ->
    _pipe = Cause,
    _pipe@1 = gleam@list:index_map(
        _pipe,
        fun(Line, Index) ->
            gleam@string:concat(
                [<<"  "/utf8>>,
                    gleam@int:to_string(Index),
                    <<": "/utf8>>,
                    Line,
                    <<"\n"/utf8>>]
            )
        end
    ),
    gleam@string_builder:from_strings(_pipe@1).

-spec pretty_print(snag()) -> binary().
pretty_print(Snag) ->
    Builder = gleam@string_builder:from_strings(
        [<<"error: "/utf8>>, erlang:element(2, Snag), <<"\n"/utf8>>]
    ),
    gleam@string_builder:to_string(case erlang:element(3, Snag) of
            [] ->
                Builder;

            Cause ->
                _pipe = Builder,
                _pipe@1 = gleam@string_builder:append(
                    _pipe,
                    <<"\ncause:\n"/utf8>>
                ),
                gleam@string_builder:append_builder(
                    _pipe@1,
                    pretty_print_cause(Cause)
                )
        end).

-spec line_print(snag()) -> binary().
line_print(Snag) ->
    _pipe = [gleam@string:append(<<"error: "/utf8>>, erlang:element(2, Snag)) |
        erlang:element(3, Snag)],
    gleam@string:join(_pipe, <<" <- "/utf8>>).
