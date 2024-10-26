-module(tom).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/2, get_int/2, get_float/2, get_bool/2, get_string/2, get_date/2, get_time/2, get_date_time/2, get_array/2, get_table/2, get_number/2, parse/1]).
-export_type([toml/0, date_time/0, date/0, time/0, offset/0, sign/0, parse_error/0, number_/0, get_error/0]).

-type toml() :: {int, integer()} |
    {float, float()} |
    {infinity, sign()} |
    {nan, sign()} |
    {bool, boolean()} |
    {string, binary()} |
    {date, date()} |
    {time, time()} |
    {date_time, date_time()} |
    {array, list(toml())} |
    {array_of_tables, list(gleam@dict:dict(binary(), toml()))} |
    {table, gleam@dict:dict(binary(), toml())} |
    {inline_table, gleam@dict:dict(binary(), toml())}.

-type date_time() :: {date_time_value, date(), time(), offset()}.

-type date() :: {date_value, integer(), integer(), integer()}.

-type time() :: {time_value, integer(), integer(), integer(), integer()}.

-type offset() :: local | {offset, sign(), integer(), integer()}.

-type sign() :: positive | negative.

-type parse_error() :: {unexpected, binary(), binary()} |
    {key_already_in_use, list(binary())}.

-type number_() :: {number_int, integer()} |
    {number_float, float()} |
    {number_infinity, sign()} |
    {number_nan, sign()}.

-type get_error() :: {not_found, list(binary())} |
    {wrong_type, list(binary()), binary(), binary()}.

-spec classify(toml()) -> binary().
classify(Toml) ->
    case Toml of
        {int, _} ->
            <<"Int"/utf8>>;

        {float, _} ->
            <<"Float"/utf8>>;

        {nan, positive} ->
            <<"NaN"/utf8>>;

        {nan, negative} ->
            <<"Negative NaN"/utf8>>;

        {infinity, positive} ->
            <<"Infinity"/utf8>>;

        {infinity, negative} ->
            <<"Negative Infinity"/utf8>>;

        {bool, _} ->
            <<"Bool"/utf8>>;

        {string, _} ->
            <<"String"/utf8>>;

        {date, _} ->
            <<"Date"/utf8>>;

        {time, _} ->
            <<"Time"/utf8>>;

        {date_time, _} ->
            <<"DateTime"/utf8>>;

        {array, _} ->
            <<"Array"/utf8>>;

        {array_of_tables, _} ->
            <<"Array"/utf8>>;

        {table, _} ->
            <<"Table"/utf8>>;

        {inline_table, _} ->
            <<"Table"/utf8>>
    end.

-spec push_key({ok, FMH} | {error, get_error()}, binary()) -> {ok, FMH} |
    {error, get_error()}.
push_key(Result, Key) ->
    case Result of
        {ok, T} ->
            {ok, T};

        {error, {not_found, Path}} ->
            {error, {not_found, [Key | Path]}};

        {error, {wrong_type, Path@1, Expected, Got}} ->
            {error, {wrong_type, [Key | Path@1], Expected, Got}}
    end.

-spec get(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok, toml()} |
    {error, get_error()}.
get(Toml, Key) ->
    case Key of
        [] ->
            {error, {not_found, []}};

        [K] ->
            gleam@result:replace_error(
                gleam@dict:get(Toml, K),
                {not_found, [K]}
            );

        [K@1 | Key@1] ->
            case gleam@dict:get(Toml, K@1) of
                {ok, {table, T}} ->
                    push_key(get(T, Key@1), K@1);

                {ok, {inline_table, T@1}} ->
                    push_key(get(T@1, Key@1), K@1);

                {ok, Other} ->
                    {error,
                        {wrong_type, [K@1], <<"Table"/utf8>>, classify(Other)}};

                {error, _} ->
                    {error, {not_found, [K@1]}}
            end
    end.

-spec get_int(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        integer()} |
    {error, get_error()}.
get_int(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {int, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Int"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_float(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        float()} |
    {error, get_error()}.
get_float(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {float, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Float"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_bool(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        boolean()} |
    {error, get_error()}.
get_bool(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {bool, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Bool"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_string(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        binary()} |
    {error, get_error()}.
get_string(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {string, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"String"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_date(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        date()} |
    {error, get_error()}.
get_date(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {date, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Date"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_time(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        time()} |
    {error, get_error()}.
get_time(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {time, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Time"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_date_time(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        date_time()} |
    {error, get_error()}.
get_date_time(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {date_time, I}} ->
            {ok, I};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"DateTime"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_array(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        list(toml())} |
    {error, get_error()}.
get_array(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {array, I}} ->
            {ok, I};

        {ok, {array_of_tables, I@1}} ->
            {ok, gleam@list:map(I@1, fun(Field@0) -> {table, Field@0} end)};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Array"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_table(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, get_error()}.
get_table(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {table, I}} ->
            {ok, I};

        {ok, {inline_table, I@1}} ->
            {ok, I@1};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Table"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec get_number(gleam@dict:dict(binary(), toml()), list(binary())) -> {ok,
        number_()} |
    {error, get_error()}.
get_number(Toml, Key) ->
    case get(Toml, Key) of
        {ok, {int, X}} ->
            {ok, {number_int, X}};

        {ok, {float, X@1}} ->
            {ok, {number_float, X@1}};

        {ok, {nan, X@2}} ->
            {ok, {number_nan, X@2}};

        {ok, {infinity, X@3}} ->
            {ok, {number_infinity, X@3}};

        {ok, Other} ->
            {error, {wrong_type, Key, <<"Number"/utf8>>, classify(Other)}};

        {error, E} ->
            {error, E}
    end.

-spec merge(gleam@dict:dict(binary(), toml()), binary(), toml(), toml()) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, list(binary())}.
merge(Table, Key, Old, New) ->
    case {Old, New} of
        {{array_of_tables, Tables}, {array_of_tables, New@1}} ->
            {ok,
                gleam@dict:insert(
                    Table,
                    Key,
                    {array_of_tables, gleam@list:append(New@1, Tables)}
                )};

        {_, _} ->
            {error, [Key]}
    end.

-spec insert_loop(gleam@dict:dict(binary(), toml()), list(binary()), toml()) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, list(binary())}.
insert_loop(Table, Key, Value) ->
    case Key of
        [] ->
            erlang:error(#{gleam_error => panic,
                    message => <<"unreachable"/utf8>>,
                    module => <<"tom"/utf8>>,
                    function => <<"insert_loop"/utf8>>,
                    line => 515});

        [K] ->
            case gleam@dict:get(Table, K) of
                {error, nil} ->
                    {ok, gleam@dict:insert(Table, K, Value)};

                {ok, Old} ->
                    merge(Table, K, Old, Value)
            end;

        [K@1 | Key@1] ->
            case gleam@dict:get(Table, K@1) of
                {error, nil} ->
                    case insert_loop(gleam@dict:new(), Key@1, Value) of
                        {ok, Inner} ->
                            {ok, gleam@dict:insert(Table, K@1, {table, Inner})};

                        {error, Path} ->
                            {error, [K@1 | Path]}
                    end;

                {ok, {array_of_tables, [Inner@1 | Rest]}} ->
                    case insert_loop(Inner@1, Key@1, Value) of
                        {ok, Inner@2} ->
                            {ok,
                                gleam@dict:insert(
                                    Table,
                                    K@1,
                                    {array_of_tables, [Inner@2 | Rest]}
                                )};

                        {error, Path@1} ->
                            {error, [K@1 | Path@1]}
                    end;

                {ok, {table, Inner@3}} ->
                    case insert_loop(Inner@3, Key@1, Value) of
                        {ok, Inner@4} ->
                            {ok,
                                gleam@dict:insert(Table, K@1, {table, Inner@4})};

                        {error, Path@2} ->
                            {error, [K@1 | Path@2]}
                    end;

                {ok, _} ->
                    {error, [K@1]}
            end
    end.

-spec insert(gleam@dict:dict(binary(), toml()), list(binary()), toml()) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, parse_error()}.
insert(Table, Key, Value) ->
    case insert_loop(Table, Key, Value) of
        {ok, Table@1} ->
            {ok, Table@1};

        {error, Path} ->
            {error, {key_already_in_use, Path}}
    end.

-spec expect_end_of_line(
    list(binary()),
    fun((list(binary())) -> {ok, {FOM, list(binary())}} | {error, parse_error()})
) -> {ok, {FOM, list(binary())}} | {error, parse_error()}.
expect_end_of_line(Input, Next) ->
    case Input of
        [<<"\n"/utf8>> | Input@1] ->
            Next(Input@1);

        [<<"\r\n"/utf8>> | Input@2] ->
            Next(Input@2);

        [G | _] ->
            {error, {unexpected, G, <<"\n"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\n"/utf8>>}}
    end.

-spec parse_key_quoted(list(binary()), binary(), binary()) -> {ok,
        {binary(), list(binary())}} |
    {error, parse_error()}.
parse_key_quoted(Input, Close, Name) ->
    case Input of
        [G | Input@1] when G =:= Close ->
            {ok, {Name, Input@1}};

        [G@1 | Input@2] ->
            parse_key_quoted(Input@2, Close, <<Name/binary, G@1/binary>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, Close}}
    end.

-spec parse_key_bare(list(binary()), binary()) -> {ok,
        {binary(), list(binary())}} |
    {error, parse_error()}.
parse_key_bare(Input, Name) ->
    case Input of
        [<<" "/utf8>> | Input@1] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input@1}};

        [<<"="/utf8>> | _] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input}};

        [<<"."/utf8>> | _] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input}};

        [<<"]"/utf8>> | _] when Name =/= <<""/utf8>> ->
            {ok, {Name, Input}};

        [<<","/utf8>> | _] when Name =/= <<""/utf8>> ->
            {error, {unexpected, <<","/utf8>>, <<"="/utf8>>}};

        [<<"\n"/utf8>> | _] when Name =/= <<""/utf8>> ->
            {error, {unexpected, <<"\n"/utf8>>, <<"="/utf8>>}};

        [<<"\r\n"/utf8>> | _] when Name =/= <<""/utf8>> ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"="/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"key"/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"key"/utf8>>}};

        [<<"]"/utf8>> | _] ->
            {error, {unexpected, <<"]"/utf8>>, <<"key"/utf8>>}};

        [<<","/utf8>> | _] ->
            {error, {unexpected, <<","/utf8>>, <<"key"/utf8>>}};

        [G | Input@2] ->
            parse_key_bare(Input@2, <<Name/binary, G/binary>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"key"/utf8>>}}
    end.

-spec skip_line_whitespace(list(binary())) -> list(binary()).
skip_line_whitespace(Input) ->
    gleam@list:drop_while(
        Input,
        fun(G) -> (G =:= <<" "/utf8>>) orelse (G =:= <<"\t"/utf8>>) end
    ).

-spec parse_key_segment(list(binary())) -> {ok, {binary(), list(binary())}} |
    {error, parse_error()}.
parse_key_segment(Input) ->
    Input@1 = skip_line_whitespace(Input),
    case Input@1 of
        [<<"="/utf8>> | _] ->
            {error, {unexpected, <<"="/utf8>>, <<"Key"/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"Key"/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"Key"/utf8>>}};

        [<<"["/utf8>> | _] ->
            {error, {unexpected, <<"["/utf8>>, <<"Key"/utf8>>}};

        [<<"\""/utf8>> | Input@2] ->
            parse_key_quoted(Input@2, <<"\""/utf8>>, <<""/utf8>>);

        [<<"'"/utf8>> | Input@3] ->
            parse_key_quoted(Input@3, <<"'"/utf8>>, <<""/utf8>>);

        _ ->
            parse_key_bare(Input@1, <<""/utf8>>)
    end.

-spec skip_whitespace(list(binary())) -> list(binary()).
skip_whitespace(Input) ->
    case Input of
        [<<" "/utf8>> | Input@1] ->
            skip_whitespace(Input@1);

        [<<"\t"/utf8>> | Input@2] ->
            skip_whitespace(Input@2);

        [<<"\n"/utf8>> | Input@3] ->
            skip_whitespace(Input@3);

        [<<"\r\n"/utf8>> | Input@4] ->
            skip_whitespace(Input@4);

        Input@5 ->
            Input@5
    end.

-spec drop_comments(list(binary()), list(binary()), boolean()) -> list(binary()).
drop_comments(Input, Acc, In_string) ->
    case Input of
        [<<"\\"/utf8>>, <<"\""/utf8>> | Input@1] when In_string ->
            drop_comments(
                Input@1,
                [<<"\""/utf8>>, <<"\\"/utf8>> | Acc],
                In_string
            );

        [<<"\""/utf8>> | Input@2] ->
            drop_comments(Input@2, [<<"\""/utf8>> | Acc], not In_string);

        [<<"#"/utf8>> | Input@3] when In_string ->
            drop_comments(Input@3, [<<"#"/utf8>> | Acc], In_string);

        [<<"#"/utf8>> | Input@4] when not In_string ->
            _pipe = Input@4,
            _pipe@1 = gleam@list:drop_while(
                _pipe,
                fun(G) -> G /= <<"\n"/utf8>> end
            ),
            drop_comments(_pipe@1, Acc, In_string);

        [G@1 | Input@5] ->
            drop_comments(Input@5, [G@1 | Acc], In_string);

        [] ->
            gleam@list:reverse(Acc)
    end.

-spec do(
    {ok, {FOX, list(binary())}} | {error, parse_error()},
    fun((FOX, list(binary())) -> {ok, FPA} | {error, parse_error()})
) -> {ok, FPA} | {error, parse_error()}.
do(Result, Next) ->
    case Result of
        {ok, {A, Input}} ->
            Next(A, Input);

        {error, E} ->
            {error, E}
    end.

-spec parse_key(list(binary()), list(binary())) -> {ok,
        {list(binary()), list(binary())}} |
    {error, parse_error()}.
parse_key(Input, Segments) ->
    do(
        parse_key_segment(Input),
        fun(Segment, Input@1) ->
            Segments@1 = [Segment | Segments],
            Input@2 = skip_line_whitespace(Input@1),
            case Input@2 of
                [<<"."/utf8>> | Input@3] ->
                    parse_key(Input@3, Segments@1);

                _ ->
                    {ok, {gleam@list:reverse(Segments@1), Input@2}}
            end
        end
    ).

-spec expect(
    list(binary()),
    binary(),
    fun((list(binary())) -> {ok, {FPF, list(binary())}} | {error, parse_error()})
) -> {ok, {FPF, list(binary())}} | {error, parse_error()}.
expect(Input, Expected, Next) ->
    case Input of
        [G | Input@1] when G =:= Expected ->
            Next(Input@1);

        [G@1 | _] ->
            {error, {unexpected, G@1, Expected}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, Expected}}
    end.

-spec parse_table_header(list(binary())) -> {ok,
        {list(binary()), list(binary())}} |
    {error, parse_error()}.
parse_table_header(Input) ->
    Input@1 = skip_line_whitespace(Input),
    do(
        parse_key(Input@1, []),
        fun(Key, Input@2) ->
            expect(
                Input@2,
                <<"]"/utf8>>,
                fun(Input@3) ->
                    Input@4 = skip_line_whitespace(Input@3),
                    expect_end_of_line(
                        Input@4,
                        fun(Input@5) -> {ok, {Key, Input@5}} end
                    )
                end
            )
        end
    ).

-spec parse_hex(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_hex(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_hex(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_hex(Input@2, (Number * 16) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_hex(Input@3, (Number * 16) + 1, Sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_hex(Input@4, (Number * 16) + 2, Sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_hex(Input@5, (Number * 16) + 3, Sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_hex(Input@6, (Number * 16) + 4, Sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_hex(Input@7, (Number * 16) + 5, Sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_hex(Input@8, (Number * 16) + 6, Sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_hex(Input@9, (Number * 16) + 7, Sign);

        [<<"8"/utf8>> | Input@10] ->
            parse_hex(Input@10, (Number * 16) + 8, Sign);

        [<<"9"/utf8>> | Input@11] ->
            parse_hex(Input@11, (Number * 16) + 9, Sign);

        [<<"a"/utf8>> | Input@12] ->
            parse_hex(Input@12, (Number * 16) + 10, Sign);

        [<<"b"/utf8>> | Input@13] ->
            parse_hex(Input@13, (Number * 16) + 11, Sign);

        [<<"c"/utf8>> | Input@14] ->
            parse_hex(Input@14, (Number * 16) + 12, Sign);

        [<<"d"/utf8>> | Input@15] ->
            parse_hex(Input@15, (Number * 16) + 13, Sign);

        [<<"e"/utf8>> | Input@16] ->
            parse_hex(Input@16, (Number * 16) + 14, Sign);

        [<<"f"/utf8>> | Input@17] ->
            parse_hex(Input@17, (Number * 16) + 15, Sign);

        [<<"A"/utf8>> | Input@18] ->
            parse_hex(Input@18, (Number * 16) + 10, Sign);

        [<<"B"/utf8>> | Input@19] ->
            parse_hex(Input@19, (Number * 16) + 11, Sign);

        [<<"C"/utf8>> | Input@20] ->
            parse_hex(Input@20, (Number * 16) + 12, Sign);

        [<<"D"/utf8>> | Input@21] ->
            parse_hex(Input@21, (Number * 16) + 13, Sign);

        [<<"E"/utf8>> | Input@22] ->
            parse_hex(Input@22, (Number * 16) + 14, Sign);

        [<<"F"/utf8>> | Input@23] ->
            parse_hex(Input@23, (Number * 16) + 15, Sign);

        Input@24 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@24}}
    end.

-spec parse_octal(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_octal(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_octal(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_octal(Input@2, (Number * 8) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_octal(Input@3, (Number * 8) + 1, Sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_octal(Input@4, (Number * 8) + 2, Sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_octal(Input@5, (Number * 8) + 3, Sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_octal(Input@6, (Number * 8) + 4, Sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_octal(Input@7, (Number * 8) + 5, Sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_octal(Input@8, (Number * 8) + 6, Sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_octal(Input@9, (Number * 8) + 7, Sign);

        Input@10 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@10}}
    end.

-spec parse_binary(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_binary(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_binary(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_binary(Input@2, (Number * 2) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_binary(Input@3, (Number * 2) + 1, Sign);

        Input@4 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@4}}
    end.

-spec parse_exponent(list(binary()), float(), sign(), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_exponent(Input, N, N_sign, Ex, Ex_sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_exponent(Input@1, N, N_sign, Ex, Ex_sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_exponent(Input@2, N, N_sign, Ex * 10, Ex_sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_exponent(Input@3, N, N_sign, (Ex * 10) + 1, Ex_sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_exponent(Input@4, N, N_sign, (Ex * 10) + 2, Ex_sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_exponent(Input@5, N, N_sign, (Ex * 10) + 3, Ex_sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_exponent(Input@6, N, N_sign, (Ex * 10) + 4, Ex_sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_exponent(Input@7, N, N_sign, (Ex * 10) + 5, Ex_sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_exponent(Input@8, N, N_sign, (Ex * 10) + 6, Ex_sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_exponent(Input@9, N, N_sign, (Ex * 10) + 7, Ex_sign);

        [<<"8"/utf8>> | Input@10] ->
            parse_exponent(Input@10, N, N_sign, (Ex * 10) + 8, Ex_sign);

        [<<"9"/utf8>> | Input@11] ->
            parse_exponent(Input@11, N, N_sign, (Ex * 10) + 9, Ex_sign);

        Input@12 ->
            Number = case N_sign of
                positive ->
                    N;

                negative ->
                    N * -1.0
            end,
            Exponent = gleam@int:to_float(case Ex_sign of
                    positive ->
                        Ex;

                    negative ->
                        - Ex
                end),
            Multiplier@1 = case gleam@float:power(10.0, Exponent) of
                {ok, Multiplier} ->
                    Multiplier;

                {error, _} ->
                    1.0
            end,
            {ok, {{float, Number * Multiplier@1}, Input@12}}
    end.

-spec parse_float(list(binary()), float(), sign(), float()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_float(Input, Number, Sign, Unit) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_float(Input@1, Number, Sign, Unit);

        [<<"0"/utf8>> | Input@2] ->
            parse_float(Input@2, Number, Sign, Unit * 0.1);

        [<<"1"/utf8>> | Input@3] ->
            parse_float(Input@3, Number + (1.0 * Unit), Sign, Unit * 0.1);

        [<<"2"/utf8>> | Input@4] ->
            parse_float(Input@4, Number + (2.0 * Unit), Sign, Unit * 0.1);

        [<<"3"/utf8>> | Input@5] ->
            parse_float(Input@5, Number + (3.0 * Unit), Sign, Unit * 0.1);

        [<<"4"/utf8>> | Input@6] ->
            parse_float(Input@6, Number + (4.0 * Unit), Sign, Unit * 0.1);

        [<<"5"/utf8>> | Input@7] ->
            parse_float(Input@7, Number + (5.0 * Unit), Sign, Unit * 0.1);

        [<<"6"/utf8>> | Input@8] ->
            parse_float(Input@8, Number + (6.0 * Unit), Sign, Unit * 0.1);

        [<<"7"/utf8>> | Input@9] ->
            parse_float(Input@9, Number + (7.0 * Unit), Sign, Unit * 0.1);

        [<<"8"/utf8>> | Input@10] ->
            parse_float(Input@10, Number + (8.0 * Unit), Sign, Unit * 0.1);

        [<<"9"/utf8>> | Input@11] ->
            parse_float(Input@11, Number + (9.0 * Unit), Sign, Unit * 0.1);

        [<<"e"/utf8>>, <<"+"/utf8>> | Input@12] ->
            parse_exponent(Input@12, Number, Sign, 0, positive);

        [<<"e"/utf8>>, <<"-"/utf8>> | Input@13] ->
            parse_exponent(Input@13, Number, Sign, 0, negative);

        [<<"e"/utf8>> | Input@14] ->
            parse_exponent(Input@14, Number, Sign, 0, positive);

        [<<"E"/utf8>>, <<"+"/utf8>> | Input@15] ->
            parse_exponent(Input@15, Number, Sign, 0, positive);

        [<<"E"/utf8>>, <<"-"/utf8>> | Input@16] ->
            parse_exponent(Input@16, Number, Sign, 0, negative);

        [<<"E"/utf8>> | Input@17] ->
            parse_exponent(Input@17, Number, Sign, 0, positive);

        Input@18 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    Number * -1.0
            end,
            {ok, {{float, Number@1}, Input@18}}
    end.

-spec parse_string(list(binary()), binary()) -> {ok, {toml(), list(binary())}} |
    {error, parse_error()}.
parse_string(Input, String) ->
    case Input of
        [<<"\""/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [<<"\\"/utf8>>, <<"t"/utf8>> | Input@2] ->
            parse_string(Input@2, <<String/binary, "\t"/utf8>>);

        [<<"\\"/utf8>>, <<"e"/utf8>> | Input@3] ->
            parse_string(Input@3, <<String/binary, "\x{001b}"/utf8>>);

        [<<"\\"/utf8>>, <<"b"/utf8>> | Input@4] ->
            parse_string(Input@4, <<String/binary, "\x{0008}"/utf8>>);

        [<<"\\"/utf8>>, <<"n"/utf8>> | Input@5] ->
            parse_string(Input@5, <<String/binary, "\n"/utf8>>);

        [<<"\\"/utf8>>, <<"r"/utf8>> | Input@6] ->
            parse_string(Input@6, <<String/binary, "\r"/utf8>>);

        [<<"\\"/utf8>>, <<"f"/utf8>> | Input@7] ->
            parse_string(Input@7, <<String/binary, "\f"/utf8>>);

        [<<"\\"/utf8>>, <<"\""/utf8>> | Input@8] ->
            parse_string(Input@8, <<String/binary, "\""/utf8>>);

        [<<"\\"/utf8>>, <<"\\"/utf8>> | Input@9] ->
            parse_string(Input@9, <<String/binary, "\\"/utf8>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"\""/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"\""/utf8>>}};

        [G | Input@10] ->
            parse_string(Input@10, <<String/binary, G/binary>>)
    end.

-spec parse_multi_line_string(list(binary()), binary()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_multi_line_string(Input, String) ->
    case Input of
        [<<"\""/utf8>>, <<"\""/utf8>>, <<"\""/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [<<"\\"/utf8>>, <<"\n"/utf8>> | Input@2] ->
            parse_multi_line_string(skip_whitespace(Input@2), String);

        [<<"\\"/utf8>>, <<"\r\n"/utf8>> | Input@3] ->
            parse_multi_line_string(skip_whitespace(Input@3), String);

        [<<"\r\n"/utf8>> | Input@4] when String =:= <<""/utf8>> ->
            parse_multi_line_string(Input@4, String);

        [<<"\n"/utf8>> | Input@5] when String =:= <<""/utf8>> ->
            parse_multi_line_string(Input@5, String);

        [<<"\r\n"/utf8>> | Input@6] when String =:= <<""/utf8>> ->
            parse_multi_line_string(Input@6, String);

        [<<"\\"/utf8>>, <<"t"/utf8>> | Input@7] ->
            parse_multi_line_string(Input@7, <<String/binary, "\t"/utf8>>);

        [<<"\\"/utf8>>, <<"n"/utf8>> | Input@8] ->
            parse_multi_line_string(Input@8, <<String/binary, "\n"/utf8>>);

        [<<"\\"/utf8>>, <<"r"/utf8>> | Input@9] ->
            parse_multi_line_string(Input@9, <<String/binary, "\r"/utf8>>);

        [<<"\\"/utf8>>, <<"\""/utf8>> | Input@10] ->
            parse_multi_line_string(Input@10, <<String/binary, "\""/utf8>>);

        [<<"\\"/utf8>>, <<"\\"/utf8>> | Input@11] ->
            parse_multi_line_string(Input@11, <<String/binary, "\\"/utf8>>);

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [G | Input@12] ->
            parse_multi_line_string(Input@12, <<String/binary, G/binary>>)
    end.

-spec parse_multi_line_literal_string(list(binary()), binary()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_multi_line_literal_string(Input, String) ->
    case Input of
        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [<<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>> | _] ->
            {error, {unexpected, <<"''''"/utf8>>, <<"'''"/utf8>>}};

        [<<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [<<"\n"/utf8>> | Input@2] when String =:= <<""/utf8>> ->
            parse_multi_line_literal_string(Input@2, String);

        [<<"\r\n"/utf8>> | Input@3] when String =:= <<""/utf8>> ->
            parse_multi_line_literal_string(Input@3, String);

        [G | Input@4] ->
            parse_multi_line_literal_string(
                Input@4,
                <<String/binary, G/binary>>
            )
    end.

-spec parse_literal_string(list(binary()), binary()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_literal_string(Input, String) ->
    case Input of
        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"\""/utf8>>}};

        [<<"\n"/utf8>> | _] ->
            {error, {unexpected, <<"\n"/utf8>>, <<"'"/utf8>>}};

        [<<"\r\n"/utf8>> | _] ->
            {error, {unexpected, <<"\r\n"/utf8>>, <<"'"/utf8>>}};

        [<<"'"/utf8>> | Input@1] ->
            {ok, {{string, String}, Input@1}};

        [G | Input@2] ->
            parse_literal_string(Input@2, <<String/binary, G/binary>>)
    end.

-spec parse_time_ms(list(binary()), integer(), integer()) -> {ok,
        {{integer(), integer()}, list(binary())}} |
    {error, parse_error()}.
parse_time_ms(Input, Seconds, Ms) ->
    case Input of
        [<<"0"/utf8>> | Input@1] when Ms < 100000 ->
            parse_time_ms(Input@1, Seconds, (Ms * 10) + 0);

        [<<"1"/utf8>> | Input@2] when Ms < 100000 ->
            parse_time_ms(Input@2, Seconds, (Ms * 10) + 1);

        [<<"2"/utf8>> | Input@3] when Ms < 100000 ->
            parse_time_ms(Input@3, Seconds, (Ms * 10) + 2);

        [<<"3"/utf8>> | Input@4] when Ms < 100000 ->
            parse_time_ms(Input@4, Seconds, (Ms * 10) + 3);

        [<<"4"/utf8>> | Input@5] when Ms < 100000 ->
            parse_time_ms(Input@5, Seconds, (Ms * 10) + 4);

        [<<"5"/utf8>> | Input@6] when Ms < 100000 ->
            parse_time_ms(Input@6, Seconds, (Ms * 10) + 5);

        [<<"6"/utf8>> | Input@7] when Ms < 100000 ->
            parse_time_ms(Input@7, Seconds, (Ms * 10) + 6);

        [<<"7"/utf8>> | Input@8] when Ms < 100000 ->
            parse_time_ms(Input@8, Seconds, (Ms * 10) + 7);

        [<<"8"/utf8>> | Input@9] when Ms < 100000 ->
            parse_time_ms(Input@9, Seconds, (Ms * 10) + 8);

        [<<"9"/utf8>> | Input@10] when Ms < 100000 ->
            parse_time_ms(Input@10, Seconds, (Ms * 10) + 9);

        _ ->
            {ok, {{Seconds, Ms}, Input}}
    end.

-spec parse_number_under_60(list(binary()), binary()) -> {ok,
        {integer(), list(binary())}} |
    {error, parse_error()}.
parse_number_under_60(Input, Expected) ->
    case Input of
        [<<"0"/utf8>>, <<"0"/utf8>> | Input@1] ->
            {ok, {0, Input@1}};

        [<<"0"/utf8>>, <<"1"/utf8>> | Input@2] ->
            {ok, {1, Input@2}};

        [<<"0"/utf8>>, <<"2"/utf8>> | Input@3] ->
            {ok, {2, Input@3}};

        [<<"0"/utf8>>, <<"3"/utf8>> | Input@4] ->
            {ok, {3, Input@4}};

        [<<"0"/utf8>>, <<"4"/utf8>> | Input@5] ->
            {ok, {4, Input@5}};

        [<<"0"/utf8>>, <<"5"/utf8>> | Input@6] ->
            {ok, {5, Input@6}};

        [<<"0"/utf8>>, <<"6"/utf8>> | Input@7] ->
            {ok, {6, Input@7}};

        [<<"0"/utf8>>, <<"7"/utf8>> | Input@8] ->
            {ok, {7, Input@8}};

        [<<"0"/utf8>>, <<"8"/utf8>> | Input@9] ->
            {ok, {8, Input@9}};

        [<<"0"/utf8>>, <<"9"/utf8>> | Input@10] ->
            {ok, {9, Input@10}};

        [<<"1"/utf8>>, <<"0"/utf8>> | Input@11] ->
            {ok, {10, Input@11}};

        [<<"1"/utf8>>, <<"1"/utf8>> | Input@12] ->
            {ok, {11, Input@12}};

        [<<"1"/utf8>>, <<"2"/utf8>> | Input@13] ->
            {ok, {12, Input@13}};

        [<<"1"/utf8>>, <<"3"/utf8>> | Input@14] ->
            {ok, {13, Input@14}};

        [<<"1"/utf8>>, <<"4"/utf8>> | Input@15] ->
            {ok, {14, Input@15}};

        [<<"1"/utf8>>, <<"5"/utf8>> | Input@16] ->
            {ok, {15, Input@16}};

        [<<"1"/utf8>>, <<"6"/utf8>> | Input@17] ->
            {ok, {16, Input@17}};

        [<<"1"/utf8>>, <<"7"/utf8>> | Input@18] ->
            {ok, {17, Input@18}};

        [<<"1"/utf8>>, <<"8"/utf8>> | Input@19] ->
            {ok, {18, Input@19}};

        [<<"1"/utf8>>, <<"9"/utf8>> | Input@20] ->
            {ok, {19, Input@20}};

        [<<"2"/utf8>>, <<"0"/utf8>> | Input@21] ->
            {ok, {20, Input@21}};

        [<<"2"/utf8>>, <<"1"/utf8>> | Input@22] ->
            {ok, {21, Input@22}};

        [<<"2"/utf8>>, <<"2"/utf8>> | Input@23] ->
            {ok, {22, Input@23}};

        [<<"2"/utf8>>, <<"3"/utf8>> | Input@24] ->
            {ok, {23, Input@24}};

        [<<"2"/utf8>>, <<"4"/utf8>> | Input@25] ->
            {ok, {24, Input@25}};

        [<<"2"/utf8>>, <<"5"/utf8>> | Input@26] ->
            {ok, {25, Input@26}};

        [<<"2"/utf8>>, <<"6"/utf8>> | Input@27] ->
            {ok, {26, Input@27}};

        [<<"2"/utf8>>, <<"7"/utf8>> | Input@28] ->
            {ok, {27, Input@28}};

        [<<"2"/utf8>>, <<"8"/utf8>> | Input@29] ->
            {ok, {28, Input@29}};

        [<<"2"/utf8>>, <<"9"/utf8>> | Input@30] ->
            {ok, {29, Input@30}};

        [<<"3"/utf8>>, <<"0"/utf8>> | Input@31] ->
            {ok, {30, Input@31}};

        [<<"3"/utf8>>, <<"1"/utf8>> | Input@32] ->
            {ok, {31, Input@32}};

        [<<"3"/utf8>>, <<"2"/utf8>> | Input@33] ->
            {ok, {32, Input@33}};

        [<<"3"/utf8>>, <<"3"/utf8>> | Input@34] ->
            {ok, {33, Input@34}};

        [<<"3"/utf8>>, <<"4"/utf8>> | Input@35] ->
            {ok, {34, Input@35}};

        [<<"3"/utf8>>, <<"5"/utf8>> | Input@36] ->
            {ok, {35, Input@36}};

        [<<"3"/utf8>>, <<"6"/utf8>> | Input@37] ->
            {ok, {36, Input@37}};

        [<<"3"/utf8>>, <<"7"/utf8>> | Input@38] ->
            {ok, {37, Input@38}};

        [<<"3"/utf8>>, <<"8"/utf8>> | Input@39] ->
            {ok, {38, Input@39}};

        [<<"3"/utf8>>, <<"9"/utf8>> | Input@40] ->
            {ok, {39, Input@40}};

        [<<"4"/utf8>>, <<"0"/utf8>> | Input@41] ->
            {ok, {40, Input@41}};

        [<<"4"/utf8>>, <<"1"/utf8>> | Input@42] ->
            {ok, {41, Input@42}};

        [<<"4"/utf8>>, <<"2"/utf8>> | Input@43] ->
            {ok, {42, Input@43}};

        [<<"4"/utf8>>, <<"3"/utf8>> | Input@44] ->
            {ok, {43, Input@44}};

        [<<"4"/utf8>>, <<"4"/utf8>> | Input@45] ->
            {ok, {44, Input@45}};

        [<<"4"/utf8>>, <<"5"/utf8>> | Input@46] ->
            {ok, {45, Input@46}};

        [<<"4"/utf8>>, <<"6"/utf8>> | Input@47] ->
            {ok, {46, Input@47}};

        [<<"4"/utf8>>, <<"7"/utf8>> | Input@48] ->
            {ok, {47, Input@48}};

        [<<"4"/utf8>>, <<"8"/utf8>> | Input@49] ->
            {ok, {48, Input@49}};

        [<<"4"/utf8>>, <<"9"/utf8>> | Input@50] ->
            {ok, {49, Input@50}};

        [<<"5"/utf8>>, <<"0"/utf8>> | Input@51] ->
            {ok, {50, Input@51}};

        [<<"5"/utf8>>, <<"1"/utf8>> | Input@52] ->
            {ok, {51, Input@52}};

        [<<"5"/utf8>>, <<"2"/utf8>> | Input@53] ->
            {ok, {52, Input@53}};

        [<<"5"/utf8>>, <<"3"/utf8>> | Input@54] ->
            {ok, {53, Input@54}};

        [<<"5"/utf8>>, <<"4"/utf8>> | Input@55] ->
            {ok, {54, Input@55}};

        [<<"5"/utf8>>, <<"5"/utf8>> | Input@56] ->
            {ok, {55, Input@56}};

        [<<"5"/utf8>>, <<"6"/utf8>> | Input@57] ->
            {ok, {56, Input@57}};

        [<<"5"/utf8>>, <<"7"/utf8>> | Input@58] ->
            {ok, {57, Input@58}};

        [<<"5"/utf8>>, <<"8"/utf8>> | Input@59] ->
            {ok, {58, Input@59}};

        [<<"5"/utf8>>, <<"9"/utf8>> | Input@60] ->
            {ok, {59, Input@60}};

        [G | _] ->
            {error, {unexpected, G, Expected}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, Expected}}
    end.

-spec parse_hour_minute(list(binary())) -> {ok,
        {{integer(), integer()}, list(binary())}} |
    {error, parse_error()}.
parse_hour_minute(Input) ->
    do(case Input of
            [<<"0"/utf8>>, <<"0"/utf8>>, <<":"/utf8>> | Input@1] ->
                {ok, {0, Input@1}};

            [<<"0"/utf8>>, <<"1"/utf8>>, <<":"/utf8>> | Input@2] ->
                {ok, {1, Input@2}};

            [<<"0"/utf8>>, <<"2"/utf8>>, <<":"/utf8>> | Input@3] ->
                {ok, {2, Input@3}};

            [<<"0"/utf8>>, <<"3"/utf8>>, <<":"/utf8>> | Input@4] ->
                {ok, {3, Input@4}};

            [<<"0"/utf8>>, <<"4"/utf8>>, <<":"/utf8>> | Input@5] ->
                {ok, {4, Input@5}};

            [<<"0"/utf8>>, <<"5"/utf8>>, <<":"/utf8>> | Input@6] ->
                {ok, {5, Input@6}};

            [<<"0"/utf8>>, <<"6"/utf8>>, <<":"/utf8>> | Input@7] ->
                {ok, {6, Input@7}};

            [<<"0"/utf8>>, <<"7"/utf8>>, <<":"/utf8>> | Input@8] ->
                {ok, {7, Input@8}};

            [<<"0"/utf8>>, <<"8"/utf8>>, <<":"/utf8>> | Input@9] ->
                {ok, {8, Input@9}};

            [<<"0"/utf8>>, <<"9"/utf8>>, <<":"/utf8>> | Input@10] ->
                {ok, {9, Input@10}};

            [<<"1"/utf8>>, <<"0"/utf8>>, <<":"/utf8>> | Input@11] ->
                {ok, {10, Input@11}};

            [<<"1"/utf8>>, <<"1"/utf8>>, <<":"/utf8>> | Input@12] ->
                {ok, {11, Input@12}};

            [<<"1"/utf8>>, <<"2"/utf8>>, <<":"/utf8>> | Input@13] ->
                {ok, {12, Input@13}};

            [<<"1"/utf8>>, <<"3"/utf8>>, <<":"/utf8>> | Input@14] ->
                {ok, {13, Input@14}};

            [<<"1"/utf8>>, <<"4"/utf8>>, <<":"/utf8>> | Input@15] ->
                {ok, {14, Input@15}};

            [<<"1"/utf8>>, <<"5"/utf8>>, <<":"/utf8>> | Input@16] ->
                {ok, {15, Input@16}};

            [<<"1"/utf8>>, <<"6"/utf8>>, <<":"/utf8>> | Input@17] ->
                {ok, {16, Input@17}};

            [<<"1"/utf8>>, <<"7"/utf8>>, <<":"/utf8>> | Input@18] ->
                {ok, {17, Input@18}};

            [<<"1"/utf8>>, <<"8"/utf8>>, <<":"/utf8>> | Input@19] ->
                {ok, {18, Input@19}};

            [<<"1"/utf8>>, <<"9"/utf8>>, <<":"/utf8>> | Input@20] ->
                {ok, {19, Input@20}};

            [<<"2"/utf8>>, <<"0"/utf8>>, <<":"/utf8>> | Input@21] ->
                {ok, {20, Input@21}};

            [<<"2"/utf8>>, <<"1"/utf8>>, <<":"/utf8>> | Input@22] ->
                {ok, {21, Input@22}};

            [<<"2"/utf8>>, <<"2"/utf8>>, <<":"/utf8>> | Input@23] ->
                {ok, {22, Input@23}};

            [<<"2"/utf8>>, <<"3"/utf8>>, <<":"/utf8>> | Input@24] ->
                {ok, {23, Input@24}};

            [G | _] ->
                {error, {unexpected, G, <<"time"/utf8>>}};

            [] ->
                {error, {unexpected, <<"EOF"/utf8>>, <<"time"/utf8>>}}
        end, fun(Hours, Input@25) ->
            do(
                parse_number_under_60(Input@25, <<"minutes"/utf8>>),
                fun(Minutes, Input@26) -> {ok, {{Hours, Minutes}, Input@26}} end
            )
        end).

-spec parse_time_s_ms(list(binary())) -> {ok,
        {{integer(), integer()}, list(binary())}} |
    {error, parse_error()}.
parse_time_s_ms(Input) ->
    case Input of
        [<<":"/utf8>> | Input@1] ->
            do(
                parse_number_under_60(Input@1, <<"seconds"/utf8>>),
                fun(Seconds, Input@2) -> case Input@2 of
                        [<<"."/utf8>> | Input@3] ->
                            parse_time_ms(Input@3, Seconds, 0);

                        _ ->
                            {ok, {{Seconds, 0}, Input@2}}
                    end end
            );

        _ ->
            {ok, {{0, 0}, Input}}
    end.

-spec parse_time_minute(list(binary()), integer()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_time_minute(Input, Hours) ->
    do(
        parse_number_under_60(Input, <<"minutes"/utf8>>),
        fun(Minutes, Input@1) ->
            do(
                parse_time_s_ms(Input@1),
                fun(_use0, Input@2) ->
                    {Seconds, Ms} = _use0,
                    Time = {time_value, Hours, Minutes, Seconds, Ms},
                    {ok, {{time, Time}, Input@2}}
                end
            )
        end
    ).

-spec parse_time_value(list(binary())) -> {ok, {time(), list(binary())}} |
    {error, parse_error()}.
parse_time_value(Input) ->
    do(
        parse_hour_minute(Input),
        fun(_use0, Input@1) ->
            {Hours, Minutes} = _use0,
            do(
                parse_time_s_ms(Input@1),
                fun(_use0@1, Input@2) ->
                    {Seconds, Ms} = _use0@1,
                    Time = {time_value, Hours, Minutes, Seconds, Ms},
                    {ok, {Time, Input@2}}
                end
            )
        end
    ).

-spec parse_offset_hours(list(binary()), sign()) -> {ok,
        {offset(), list(binary())}} |
    {error, parse_error()}.
parse_offset_hours(Input, Sign) ->
    do(
        parse_hour_minute(Input),
        fun(_use0, Input@1) ->
            {Hours, Minutes} = _use0,
            {ok, {{offset, Sign, Hours, Minutes}, Input@1}}
        end
    ).

-spec parse_offset(list(binary())) -> {ok, {offset(), list(binary())}} |
    {error, parse_error()}.
parse_offset(Input) ->
    case Input of
        [<<"Z"/utf8>> | Input@1] ->
            {ok, {{offset, positive, 0, 0}, Input@1}};

        [<<"+"/utf8>> | Input@2] ->
            parse_offset_hours(Input@2, positive);

        [<<"-"/utf8>> | Input@3] ->
            parse_offset_hours(Input@3, negative);

        _ ->
            {ok, {local, Input}}
    end.

-spec parse_date_end(list(binary()), integer(), integer(), integer()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_date_end(Input, Year, Month, Day) ->
    Date = {date_value, Year, Month, Day},
    case Input of
        [<<" "/utf8>> | Input@1] ->
            do(
                parse_time_value(Input@1),
                fun(Time, Input@2) ->
                    do(
                        parse_offset(Input@2),
                        fun(Offset, Input@3) ->
                            {ok,
                                {{date_time,
                                        {date_time_value, Date, Time, Offset}},
                                    Input@3}}
                        end
                    )
                end
            );

        [<<"T"/utf8>> | Input@1] ->
            do(
                parse_time_value(Input@1),
                fun(Time, Input@2) ->
                    do(
                        parse_offset(Input@2),
                        fun(Offset, Input@3) ->
                            {ok,
                                {{date_time,
                                        {date_time_value, Date, Time, Offset}},
                                    Input@3}}
                        end
                    )
                end
            );

        _ ->
            {ok, {{date, Date}, Input}}
    end.

-spec parse_date_day(list(binary()), integer(), integer()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_date_day(Input, Year, Month) ->
    case Input of
        [<<"0"/utf8>>, <<"1"/utf8>> | Input@1] ->
            parse_date_end(Input@1, Year, Month, 1);

        [<<"0"/utf8>>, <<"2"/utf8>> | Input@2] ->
            parse_date_end(Input@2, Year, Month, 2);

        [<<"0"/utf8>>, <<"3"/utf8>> | Input@3] ->
            parse_date_end(Input@3, Year, Month, 3);

        [<<"0"/utf8>>, <<"4"/utf8>> | Input@4] ->
            parse_date_end(Input@4, Year, Month, 4);

        [<<"0"/utf8>>, <<"5"/utf8>> | Input@5] ->
            parse_date_end(Input@5, Year, Month, 5);

        [<<"0"/utf8>>, <<"6"/utf8>> | Input@6] ->
            parse_date_end(Input@6, Year, Month, 6);

        [<<"0"/utf8>>, <<"7"/utf8>> | Input@7] ->
            parse_date_end(Input@7, Year, Month, 7);

        [<<"0"/utf8>>, <<"8"/utf8>> | Input@8] ->
            parse_date_end(Input@8, Year, Month, 8);

        [<<"0"/utf8>>, <<"9"/utf8>> | Input@9] ->
            parse_date_end(Input@9, Year, Month, 9);

        [<<"1"/utf8>>, <<"0"/utf8>> | Input@10] ->
            parse_date_end(Input@10, Year, Month, 10);

        [<<"1"/utf8>>, <<"1"/utf8>> | Input@11] ->
            parse_date_end(Input@11, Year, Month, 11);

        [<<"1"/utf8>>, <<"2"/utf8>> | Input@12] ->
            parse_date_end(Input@12, Year, Month, 12);

        [<<"1"/utf8>>, <<"3"/utf8>> | Input@13] ->
            parse_date_end(Input@13, Year, Month, 13);

        [<<"1"/utf8>>, <<"4"/utf8>> | Input@14] ->
            parse_date_end(Input@14, Year, Month, 14);

        [<<"1"/utf8>>, <<"5"/utf8>> | Input@15] ->
            parse_date_end(Input@15, Year, Month, 15);

        [<<"1"/utf8>>, <<"6"/utf8>> | Input@16] ->
            parse_date_end(Input@16, Year, Month, 16);

        [<<"1"/utf8>>, <<"7"/utf8>> | Input@17] ->
            parse_date_end(Input@17, Year, Month, 17);

        [<<"1"/utf8>>, <<"8"/utf8>> | Input@18] ->
            parse_date_end(Input@18, Year, Month, 18);

        [<<"1"/utf8>>, <<"9"/utf8>> | Input@19] ->
            parse_date_end(Input@19, Year, Month, 19);

        [<<"2"/utf8>>, <<"0"/utf8>> | Input@20] ->
            parse_date_end(Input@20, Year, Month, 20);

        [<<"2"/utf8>>, <<"1"/utf8>> | Input@21] ->
            parse_date_end(Input@21, Year, Month, 21);

        [<<"2"/utf8>>, <<"2"/utf8>> | Input@22] ->
            parse_date_end(Input@22, Year, Month, 22);

        [<<"2"/utf8>>, <<"3"/utf8>> | Input@23] ->
            parse_date_end(Input@23, Year, Month, 23);

        [<<"2"/utf8>>, <<"4"/utf8>> | Input@24] ->
            parse_date_end(Input@24, Year, Month, 24);

        [<<"2"/utf8>>, <<"5"/utf8>> | Input@25] ->
            parse_date_end(Input@25, Year, Month, 25);

        [<<"2"/utf8>>, <<"6"/utf8>> | Input@26] ->
            parse_date_end(Input@26, Year, Month, 26);

        [<<"2"/utf8>>, <<"7"/utf8>> | Input@27] ->
            parse_date_end(Input@27, Year, Month, 27);

        [<<"2"/utf8>>, <<"8"/utf8>> | Input@28] ->
            parse_date_end(Input@28, Year, Month, 28);

        [<<"2"/utf8>>, <<"9"/utf8>> | Input@29] ->
            parse_date_end(Input@29, Year, Month, 29);

        [<<"3"/utf8>>, <<"0"/utf8>> | Input@30] ->
            parse_date_end(Input@30, Year, Month, 30);

        [<<"3"/utf8>>, <<"1"/utf8>> | Input@31] ->
            parse_date_end(Input@31, Year, Month, 31);

        [G | _] ->
            {error, {unexpected, G, <<"date day"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"date day"/utf8>>}}
    end.

-spec parse_date(list(binary()), integer()) -> {ok, {toml(), list(binary())}} |
    {error, parse_error()}.
parse_date(Input, Year) ->
    case Input of
        [<<"0"/utf8>>, <<"1"/utf8>>, <<"-"/utf8>> | Input@1] ->
            parse_date_day(Input@1, Year, 1);

        [<<"0"/utf8>>, <<"2"/utf8>>, <<"-"/utf8>> | Input@2] ->
            parse_date_day(Input@2, Year, 2);

        [<<"0"/utf8>>, <<"3"/utf8>>, <<"-"/utf8>> | Input@3] ->
            parse_date_day(Input@3, Year, 3);

        [<<"0"/utf8>>, <<"4"/utf8>>, <<"-"/utf8>> | Input@4] ->
            parse_date_day(Input@4, Year, 4);

        [<<"0"/utf8>>, <<"5"/utf8>>, <<"-"/utf8>> | Input@5] ->
            parse_date_day(Input@5, Year, 5);

        [<<"0"/utf8>>, <<"6"/utf8>>, <<"-"/utf8>> | Input@6] ->
            parse_date_day(Input@6, Year, 6);

        [<<"0"/utf8>>, <<"7"/utf8>>, <<"-"/utf8>> | Input@7] ->
            parse_date_day(Input@7, Year, 7);

        [<<"0"/utf8>>, <<"8"/utf8>>, <<"-"/utf8>> | Input@8] ->
            parse_date_day(Input@8, Year, 8);

        [<<"0"/utf8>>, <<"9"/utf8>>, <<"-"/utf8>> | Input@9] ->
            parse_date_day(Input@9, Year, 9);

        [<<"1"/utf8>>, <<"0"/utf8>>, <<"-"/utf8>> | Input@10] ->
            parse_date_day(Input@10, Year, 10);

        [<<"1"/utf8>>, <<"1"/utf8>>, <<"-"/utf8>> | Input@11] ->
            parse_date_day(Input@11, Year, 11);

        [<<"1"/utf8>>, <<"2"/utf8>>, <<"-"/utf8>> | Input@12] ->
            parse_date_day(Input@12, Year, 12);

        [G | _] ->
            {error, {unexpected, G, <<"date month"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"date month"/utf8>>}}
    end.

-spec parse_number(list(binary()), integer(), sign()) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_number(Input, Number, Sign) ->
    case Input of
        [<<"_"/utf8>> | Input@1] ->
            parse_number(Input@1, Number, Sign);

        [<<"0"/utf8>> | Input@2] ->
            parse_number(Input@2, (Number * 10) + 0, Sign);

        [<<"1"/utf8>> | Input@3] ->
            parse_number(Input@3, (Number * 10) + 1, Sign);

        [<<"2"/utf8>> | Input@4] ->
            parse_number(Input@4, (Number * 10) + 2, Sign);

        [<<"3"/utf8>> | Input@5] ->
            parse_number(Input@5, (Number * 10) + 3, Sign);

        [<<"4"/utf8>> | Input@6] ->
            parse_number(Input@6, (Number * 10) + 4, Sign);

        [<<"5"/utf8>> | Input@7] ->
            parse_number(Input@7, (Number * 10) + 5, Sign);

        [<<"6"/utf8>> | Input@8] ->
            parse_number(Input@8, (Number * 10) + 6, Sign);

        [<<"7"/utf8>> | Input@9] ->
            parse_number(Input@9, (Number * 10) + 7, Sign);

        [<<"8"/utf8>> | Input@10] ->
            parse_number(Input@10, (Number * 10) + 8, Sign);

        [<<"9"/utf8>> | Input@11] ->
            parse_number(Input@11, (Number * 10) + 9, Sign);

        [<<"-"/utf8>> | Input@12] ->
            parse_date(Input@12, Number);

        [<<":"/utf8>> | Input@13] when Number < 24 ->
            parse_time_minute(Input@13, Number);

        [<<"."/utf8>> | Input@14] ->
            parse_float(Input@14, gleam@int:to_float(Number), Sign, 0.1);

        [<<"e"/utf8>>, <<"+"/utf8>> | Input@15] ->
            parse_exponent(
                Input@15,
                gleam@int:to_float(Number),
                Sign,
                0,
                positive
            );

        [<<"e"/utf8>>, <<"-"/utf8>> | Input@16] ->
            parse_exponent(
                Input@16,
                gleam@int:to_float(Number),
                Sign,
                0,
                negative
            );

        [<<"e"/utf8>> | Input@17] ->
            parse_exponent(
                Input@17,
                gleam@int:to_float(Number),
                Sign,
                0,
                positive
            );

        [<<"E"/utf8>>, <<"+"/utf8>> | Input@18] ->
            parse_exponent(
                Input@18,
                gleam@int:to_float(Number),
                Sign,
                0,
                positive
            );

        [<<"E"/utf8>>, <<"-"/utf8>> | Input@19] ->
            parse_exponent(
                Input@19,
                gleam@int:to_float(Number),
                Sign,
                0,
                negative
            );

        [<<"E"/utf8>> | Input@20] ->
            parse_exponent(
                Input@20,
                gleam@int:to_float(Number),
                Sign,
                0,
                positive
            );

        Input@21 ->
            Number@1 = case Sign of
                positive ->
                    Number;

                negative ->
                    - Number
            end,
            {ok, {{int, Number@1}, Input@21}}
    end.

-spec reverse_arrays_of_tables(toml()) -> toml().
reverse_arrays_of_tables(Toml) ->
    case Toml of
        {array_of_tables, Tables} ->
            {array_of_tables, reverse_arrays_of_tables_array(Tables, [])};

        {table, Table} ->
            {table, reverse_arrays_of_tables_table(Table)};

        _ ->
            Toml
    end.

-spec reverse_arrays_of_tables_table(gleam@dict:dict(binary(), toml())) -> gleam@dict:dict(binary(), toml()).
reverse_arrays_of_tables_table(Table) ->
    gleam@dict:map_values(Table, fun(_, V) -> reverse_arrays_of_tables(V) end).

-spec reverse_arrays_of_tables_array(
    list(gleam@dict:dict(binary(), toml())),
    list(gleam@dict:dict(binary(), toml()))
) -> list(gleam@dict:dict(binary(), toml())).
reverse_arrays_of_tables_array(Array, Acc) ->
    case Array of
        [] ->
            Acc;

        [First | Rest] ->
            First@1 = reverse_arrays_of_tables_table(First),
            reverse_arrays_of_tables_array(Rest, [First@1 | Acc])
    end.

-spec parse_inline_table_property(
    list(binary()),
    gleam@dict:dict(binary(), toml())
) -> {ok, {gleam@dict:dict(binary(), toml()), list(binary())}} |
    {error, parse_error()}.
parse_inline_table_property(Input, Properties) ->
    Input@1 = skip_whitespace(Input),
    do(
        parse_key(Input@1, []),
        fun(Key, Input@2) ->
            Input@3 = skip_line_whitespace(Input@2),
            expect(
                Input@3,
                <<"="/utf8>>,
                fun(Input@4) ->
                    Input@5 = skip_line_whitespace(Input@4),
                    do(
                        parse_value(Input@5),
                        fun(Value, Input@6) ->
                            case insert(Properties, Key, Value) of
                                {ok, Properties@1} ->
                                    {ok, {Properties@1, Input@6}};

                                {error, E} ->
                                    {error, E}
                            end
                        end
                    )
                end
            )
        end
    ).

-spec parse_value(list(binary())) -> {ok, {toml(), list(binary())}} |
    {error, parse_error()}.
parse_value(Input) ->
    case Input of
        [<<"t"/utf8>>, <<"r"/utf8>>, <<"u"/utf8>>, <<"e"/utf8>> | Input@1] ->
            {ok, {{bool, true}, Input@1}};

        [<<"f"/utf8>>,
            <<"a"/utf8>>,
            <<"l"/utf8>>,
            <<"s"/utf8>>,
            <<"e"/utf8>> |
            Input@2] ->
            {ok, {{bool, false}, Input@2}};

        [<<"n"/utf8>>, <<"a"/utf8>>, <<"n"/utf8>> | Input@3] ->
            {ok, {{nan, positive}, Input@3}};

        [<<"+"/utf8>>, <<"n"/utf8>>, <<"a"/utf8>>, <<"n"/utf8>> | Input@4] ->
            {ok, {{nan, positive}, Input@4}};

        [<<"-"/utf8>>, <<"n"/utf8>>, <<"a"/utf8>>, <<"n"/utf8>> | Input@5] ->
            {ok, {{nan, negative}, Input@5}};

        [<<"i"/utf8>>, <<"n"/utf8>>, <<"f"/utf8>> | Input@6] ->
            {ok, {{infinity, positive}, Input@6}};

        [<<"+"/utf8>>, <<"i"/utf8>>, <<"n"/utf8>>, <<"f"/utf8>> | Input@7] ->
            {ok, {{infinity, positive}, Input@7}};

        [<<"-"/utf8>>, <<"i"/utf8>>, <<"n"/utf8>>, <<"f"/utf8>> | Input@8] ->
            {ok, {{infinity, negative}, Input@8}};

        [<<"["/utf8>> | Input@9] ->
            parse_array(Input@9, []);

        [<<"{"/utf8>> | Input@10] ->
            parse_inline_table(Input@10, gleam@dict:new());

        [<<"0"/utf8>>, <<"x"/utf8>> | Input@11] ->
            parse_hex(Input@11, 0, positive);

        [<<"+"/utf8>>, <<"0"/utf8>>, <<"x"/utf8>> | Input@12] ->
            parse_hex(Input@12, 0, positive);

        [<<"-"/utf8>>, <<"0"/utf8>>, <<"x"/utf8>> | Input@13] ->
            parse_hex(Input@13, 0, negative);

        [<<"0"/utf8>>, <<"o"/utf8>> | Input@14] ->
            parse_octal(Input@14, 0, positive);

        [<<"+"/utf8>>, <<"0"/utf8>>, <<"o"/utf8>> | Input@15] ->
            parse_octal(Input@15, 0, positive);

        [<<"-"/utf8>>, <<"0"/utf8>>, <<"o"/utf8>> | Input@16] ->
            parse_octal(Input@16, 0, negative);

        [<<"0"/utf8>>, <<"b"/utf8>> | Input@17] ->
            parse_binary(Input@17, 0, positive);

        [<<"+"/utf8>>, <<"0"/utf8>>, <<"b"/utf8>> | Input@18] ->
            parse_binary(Input@18, 0, positive);

        [<<"-"/utf8>>, <<"0"/utf8>>, <<"b"/utf8>> | Input@19] ->
            parse_binary(Input@19, 0, negative);

        [<<"+"/utf8>> | Input@20] ->
            parse_number(Input@20, 0, positive);

        [<<"-"/utf8>> | Input@21] ->
            parse_number(Input@21, 0, negative);

        [<<"0"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"1"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"2"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"3"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"4"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"5"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"6"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"7"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"8"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"9"/utf8>> | _] ->
            parse_number(Input, 0, positive);

        [<<"\""/utf8>>, <<"\""/utf8>>, <<"\""/utf8>> | Input@22] ->
            parse_multi_line_string(Input@22, <<""/utf8>>);

        [<<"\""/utf8>> | Input@23] ->
            parse_string(Input@23, <<""/utf8>>);

        [<<"'"/utf8>>, <<"'"/utf8>>, <<"'"/utf8>> | Input@24] ->
            parse_multi_line_literal_string(Input@24, <<""/utf8>>);

        [<<"'"/utf8>> | Input@25] ->
            parse_literal_string(Input@25, <<""/utf8>>);

        [G | _] ->
            {error, {unexpected, G, <<"value"/utf8>>}};

        [] ->
            {error, {unexpected, <<"EOF"/utf8>>, <<"value"/utf8>>}}
    end.

-spec parse_inline_table(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_inline_table(Input, Properties) ->
    Input@1 = skip_whitespace(Input),
    case Input@1 of
        [<<"}"/utf8>> | Input@2] ->
            {ok, {{inline_table, Properties}, Input@2}};

        _ ->
            case parse_inline_table_property(Input@1, Properties) of
                {ok, {Properties@1, Input@3}} ->
                    Input@4 = skip_whitespace(Input@3),
                    case Input@4 of
                        [<<"}"/utf8>> | Input@5] ->
                            {ok, {{inline_table, Properties@1}, Input@5}};

                        [<<","/utf8>> | Input@6] ->
                            Input@7 = skip_whitespace(Input@6),
                            parse_inline_table(Input@7, Properties@1);

                        [G | _] ->
                            {error, {unexpected, G, <<"}"/utf8>>}};

                        [] ->
                            {error, {unexpected, <<"EOF"/utf8>>, <<"}"/utf8>>}}
                    end;

                {error, E} ->
                    {error, E}
            end
    end.

-spec parse_key_value(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        {gleam@dict:dict(binary(), toml()), list(binary())}} |
    {error, parse_error()}.
parse_key_value(Input, Toml) ->
    do(
        parse_key(Input, []),
        fun(Key, Input@1) ->
            Input@2 = skip_line_whitespace(Input@1),
            expect(
                Input@2,
                <<"="/utf8>>,
                fun(Input@3) ->
                    Input@4 = skip_line_whitespace(Input@3),
                    do(
                        parse_value(Input@4),
                        fun(Value, Input@5) -> case insert(Toml, Key, Value) of
                                {ok, Toml@1} ->
                                    {ok, {Toml@1, Input@5}};

                                {error, E} ->
                                    {error, E}
                            end end
                    )
                end
            )
        end
    ).

-spec parse_table(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        {gleam@dict:dict(binary(), toml()), list(binary())}} |
    {error, parse_error()}.
parse_table(Input, Toml) ->
    Input@1 = skip_whitespace(Input),
    case Input@1 of
        [<<"["/utf8>> | _] ->
            {ok, {Toml, Input@1}};

        [] ->
            {ok, {Toml, Input@1}};

        _ ->
            case parse_key_value(Input@1, Toml) of
                {ok, {Toml@1, Input@2}} ->
                    case skip_line_whitespace(Input@2) of
                        [] ->
                            {ok, {Toml@1, []}};

                        [<<"\n"/utf8>> | In] ->
                            parse_table(In, Toml@1);

                        [<<"\r\n"/utf8>> | In] ->
                            parse_table(In, Toml@1);

                        [G | _] ->
                            {error, {unexpected, G, <<"\n"/utf8>>}}
                    end;

                E ->
                    E
            end
    end.

-spec parse_array_of_tables(list(binary())) -> {ok,
        {{list(binary()), gleam@dict:dict(binary(), toml())}, list(binary())}} |
    {error, parse_error()}.
parse_array_of_tables(Input) ->
    Input@1 = skip_line_whitespace(Input),
    do(
        parse_key(Input@1, []),
        fun(Key, Input@2) ->
            expect(
                Input@2,
                <<"]"/utf8>>,
                fun(Input@3) ->
                    expect(
                        Input@3,
                        <<"]"/utf8>>,
                        fun(Input@4) ->
                            do(
                                parse_table(Input@4, gleam@dict:new()),
                                fun(Table, Input@5) ->
                                    {ok, {{Key, Table}, Input@5}}
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-spec parse_table_and_header(list(binary())) -> {ok,
        {{list(binary()), gleam@dict:dict(binary(), toml())}, list(binary())}} |
    {error, parse_error()}.
parse_table_and_header(Input) ->
    do(
        parse_table_header(Input),
        fun(Key, Input@1) ->
            do(
                parse_table(Input@1, gleam@dict:new()),
                fun(Table, Input@2) -> {ok, {{Key, Table}, Input@2}} end
            )
        end
    ).

-spec parse_tables(list(binary()), gleam@dict:dict(binary(), toml())) -> {ok,
        gleam@dict:dict(binary(), toml())} |
    {error, parse_error()}.
parse_tables(Input, Toml) ->
    case Input of
        [<<"["/utf8>>, <<"["/utf8>> | Input@1] ->
            case parse_array_of_tables(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, {{Key, Table}, Input@2}} ->
                    case insert(Toml, Key, {array_of_tables, [Table]}) of
                        {ok, Toml@1} ->
                            parse_tables(Input@2, Toml@1);

                        {error, E@1} ->
                            {error, E@1}
                    end
            end;

        [<<"["/utf8>> | Input@3] ->
            case parse_table_and_header(Input@3) of
                {error, E@2} ->
                    {error, E@2};

                {ok, {{Key@1, Table@1}, Input@4}} ->
                    case insert(Toml, Key@1, {table, Table@1}) of
                        {ok, Toml@2} ->
                            parse_tables(Input@4, Toml@2);

                        {error, E@3} ->
                            {error, E@3}
                    end
            end;

        [G | _] ->
            {error, {unexpected, G, <<"["/utf8>>}};

        [] ->
            {ok, Toml}
    end.

-spec parse(binary()) -> {ok, gleam@dict:dict(binary(), toml())} |
    {error, parse_error()}.
parse(Input) ->
    Input@1 = gleam@string:to_graphemes(Input),
    Input@2 = drop_comments(Input@1, [], false),
    Input@3 = skip_whitespace(Input@2),
    do(
        parse_table(Input@3, gleam@dict:new()),
        fun(Toml, Input@4) -> case parse_tables(Input@4, Toml) of
                {ok, Toml@1} ->
                    {ok, reverse_arrays_of_tables_table(Toml@1)};

                {error, E} ->
                    {error, E}
            end end
    ).

-spec parse_array(list(binary()), list(toml())) -> {ok,
        {toml(), list(binary())}} |
    {error, parse_error()}.
parse_array(Input, Elements) ->
    Input@1 = skip_whitespace(Input),
    case Input@1 of
        [<<"]"/utf8>> | Input@2] ->
            {ok, {{array, gleam@list:reverse(Elements)}, Input@2}};

        _ ->
            do(
                parse_value(Input@1),
                fun(Element, Input@3) ->
                    Elements@1 = [Element | Elements],
                    Input@4 = skip_whitespace(Input@3),
                    case Input@4 of
                        [<<"]"/utf8>> | Input@5] ->
                            {ok,
                                {{array, gleam@list:reverse(Elements@1)},
                                    Input@5}};

                        [<<","/utf8>> | Input@6] ->
                            Input@7 = skip_whitespace(Input@6),
                            parse_array(Input@7, Elements@1);

                        [G | _] ->
                            {error, {unexpected, G, <<"]"/utf8>>}};

                        [] ->
                            {error, {unexpected, <<"EOF"/utf8>>, <<"]"/utf8>>}}
                    end
                end
            )
    end.
