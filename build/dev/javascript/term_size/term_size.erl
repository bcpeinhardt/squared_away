-module(term_size).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/0, rows/0, columns/0]).

-spec get() -> {ok, {integer(), integer()}} | {error, nil}.
get() ->
    term_size_ffi:terminal_size().

-spec rows() -> {ok, integer()} | {error, nil}.
rows() ->
    case term_size_ffi:terminal_size() of
        {ok, {Rows, _}} ->
            {ok, Rows};

        {error, nil} ->
            {error, nil}
    end.

-spec columns() -> {ok, integer()} | {error, nil}.
columns() ->
    case term_size_ffi:terminal_size() of
        {ok, {_, Columns}} ->
            {ok, Columns};

        {error, nil} ->
            {error, nil}
    end.
