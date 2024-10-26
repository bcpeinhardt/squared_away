-module(mist@internal@clock).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([stop/1, start/2, get_date/0]).
-export_type([clock_message/0, clock_table/0, table_key/0, ets_opts/0]).

-type clock_message() :: set_time.

-type clock_table() :: mist_clock.

-type table_key() :: date_header.

-type ets_opts() :: set |
    protected |
    named_table |
    {read_concurrency, boolean()}.

-spec stop(any()) -> gleam@erlang@atom:atom_().
stop(_) ->
    erlang:binary_to_atom(<<"ok"/utf8>>).

-spec date() -> binary().
date() ->
    _pipe = birl:now(),
    birl:to_http(_pipe).

-spec start(any(), any()) -> {ok, gleam@erlang@process:pid_()} |
    {error, gleam@otp@actor:start_error()}.
start(_, _) ->
    _pipe@1 = gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Subj = gleam@erlang@process:new_subject(),
                Selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:selecting(
                        _pipe,
                        Subj,
                        fun gleam@function:identity/1
                    )
                end,
                ets:new(
                    mist_clock,
                    [set, protected, named_table, {read_concurrency, true}]
                ),
                gleam@erlang@process:send(Subj, set_time),
                {ready, Subj, Selector}
            end,
            500,
            fun(Msg, State) -> case Msg of
                    set_time ->
                        ets:insert(mist_clock, {date_header, date()}),
                        gleam@erlang@process:send_after(State, 1000, set_time),
                        gleam@otp@actor:continue(State)
                end end}
    ),
    gleam@result:map(_pipe@1, fun gleam@erlang@process:subject_owner/1).

-spec get_date() -> binary().
get_date() ->
    case mist_ffi:ets_lookup_element(mist_clock, date_header, 2) of
        {ok, Value} ->
            Value;

        _ ->
            logging:log(
                warning,
                <<"Failed to lookup date, re-calculating"/utf8>>
            ),
            date()
    end.
