-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BOC} | {error, BOD}, fun((BOC) -> BOG)) -> {ok, BOG} |
    {error, BOD}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BOJ} | {error, BOK}, fun((BOK) -> BON)) -> {ok, BOJ} |
    {error, BON}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BOQ} | {error, BOR}} | {error, BOR}) -> {ok, BOQ} |
    {error, BOR}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BOY} | {error, BOZ}, fun((BOY) -> {ok, BPC} | {error, BOZ})) -> {ok,
        BPC} |
    {error, BOZ}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BPH} | {error, BPI}, fun((BPH) -> {ok, BPL} | {error, BPI})) -> {ok,
        BPL} |
    {error, BPI}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BPQ} | {error, any()}, BPQ) -> BPQ.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BPU} | {error, any()}, fun(() -> BPU)) -> BPU.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BPZ}, BPZ) -> BPZ.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BQC} | {error, BQC}) -> BQC.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BQF} | {error, any()}) -> {ok, BQF} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BQL} | {error, BQM}, {ok, BQL} | {error, BQM}) -> {ok, BQL} |
    {error, BQM}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BQT} | {error, BQU}, fun(() -> {ok, BQT} | {error, BQU})) -> {ok,
        BQT} |
    {error, BQU}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BRB} | {error, BRC})) -> {ok, list(BRB)} | {error, BRC}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BRQ} | {error, BRR}), list(BRQ), list(BRR)) -> {list(BRQ),
    list(BRR)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BRJ} | {error, BRK})) -> {list(BRJ), list(BRK)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BRZ}, BSC) -> {ok, BSC} | {error, BRZ}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BSF} | {error, any()}, BSJ) -> {ok, BSF} | {error, BSJ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BSM} | {error, any()})) -> list(BSM).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BSS} | {error, BST},
    fun((BST) -> {ok, BSS} | {error, BSW})
) -> {ok, BSS} | {error, BSW}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
