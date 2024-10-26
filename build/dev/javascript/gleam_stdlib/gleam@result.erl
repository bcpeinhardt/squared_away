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

-spec map({ok, BNQ} | {error, BNR}, fun((BNQ) -> BNU)) -> {ok, BNU} |
    {error, BNR}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BNX} | {error, BNY}, fun((BNY) -> BOB)) -> {ok, BNX} |
    {error, BOB}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BOE} | {error, BOF}} | {error, BOF}) -> {ok, BOE} |
    {error, BOF}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BOM} | {error, BON}, fun((BOM) -> {ok, BOQ} | {error, BON})) -> {ok,
        BOQ} |
    {error, BON}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BOV} | {error, BOW}, fun((BOV) -> {ok, BOZ} | {error, BOW})) -> {ok,
        BOZ} |
    {error, BOW}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BPE} | {error, any()}, BPE) -> BPE.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BPI} | {error, any()}, fun(() -> BPI)) -> BPI.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BPN}, BPN) -> BPN.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BPQ} | {error, BPQ}) -> BPQ.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BPT} | {error, any()}) -> {ok, BPT} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BPZ} | {error, BQA}, {ok, BPZ} | {error, BQA}) -> {ok, BPZ} |
    {error, BQA}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BQH} | {error, BQI}, fun(() -> {ok, BQH} | {error, BQI})) -> {ok,
        BQH} |
    {error, BQI}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BQP} | {error, BQQ})) -> {ok, list(BQP)} | {error, BQQ}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BRE} | {error, BRF}), list(BRE), list(BRF)) -> {list(BRE),
    list(BRF)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BQX} | {error, BQY})) -> {list(BQX), list(BQY)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BRN}, BRQ) -> {ok, BRQ} | {error, BRN}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BRT} | {error, any()}, BRX) -> {ok, BRT} | {error, BRX}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BSA} | {error, any()})) -> list(BSA).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BSG} | {error, BSH},
    fun((BSH) -> {ok, BSG} | {error, BSK})
) -> {ok, BSG} | {error, BSK}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
