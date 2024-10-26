-module(ranger).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([create/4, create_infinite/3]).
-export_type([direction/0]).

-type direction() :: forward | backward.

-spec create(
    fun((FJS) -> boolean()),
    fun((FJT) -> FJT),
    fun((FJS, FJT) -> FJS),
    fun((FJS, FJS) -> gleam@order:order())
) -> fun((FJS, FJS, FJT) -> {ok, gleam@iterator:iterator(FJS)} | {error, nil}).
create(Validate, Negate_step, Add, Compare) ->
    Adjust_step = fun(A, B, Step) ->
        Negated_step = Negate_step(Step),
        case {Compare(A, B),
            Compare(A, Add(A, Step)),
            Compare(A, Add(A, Negated_step))} of
            {eq, _, _} ->
                {ok, none};

            {_, eq, eq} ->
                {ok, none};

            {lt, lt, _} ->
                {ok, {some, {forward, Step}}};

            {lt, _, lt} ->
                {ok, {some, {forward, Negated_step}}};

            {lt, _, _} ->
                {error, nil};

            {gt, gt, _} ->
                {ok, {some, {backward, Step}}};

            {gt, _, gt} ->
                {ok, {some, {backward, Negated_step}}};

            {gt, _, _} ->
                {error, nil}
        end
    end,
    fun(A@1, B@1, S) ->
        gleam@bool:guard(
            not Validate(A@1) orelse not Validate(B@1),
            {error, nil},
            fun() -> case Adjust_step(A@1, B@1, S) of
                    {ok, {some, {Direction, Step@1}}} ->
                        {ok,
                            gleam@iterator:unfold(
                                A@1,
                                fun(Current) ->
                                    case {Compare(Current, B@1), Direction} of
                                        {gt, forward} ->
                                            done;

                                        {lt, backward} ->
                                            done;

                                        {_, _} ->
                                            {next,
                                                Current,
                                                Add(Current, Step@1)}
                                    end
                                end
                            )};

                    {ok, none} ->
                        {ok, gleam@iterator:once(fun() -> A@1 end)};

                    {error, nil} ->
                        {error, nil}
                end end
        )
    end.

-spec create_infinite(
    fun((FJX) -> boolean()),
    fun((FJX, FJY) -> FJX),
    fun((FJX, FJX) -> gleam@order:order())
) -> fun((FJX, FJY) -> {ok, gleam@iterator:iterator(FJX)} | {error, nil}).
create_infinite(Validate, Add, Compare) ->
    Is_step_zero = fun(A, S) -> case Compare(A, Add(A, S)) of
            eq ->
                true;

            _ ->
                false
        end end,
    fun(A@1, S@1) ->
        gleam@bool:guard(
            not Validate(A@1),
            {error, nil},
            fun() ->
                gleam@bool:guard(
                    Is_step_zero(A@1, S@1),
                    begin
                        _pipe = gleam@iterator:once(fun() -> A@1 end),
                        {ok, _pipe}
                    end,
                    fun() ->
                        _pipe@1 = gleam@iterator:unfold(
                            A@1,
                            fun(Current) ->
                                {next, Current, Add(Current, S@1)}
                            end
                        ),
                        {ok, _pipe@1}
                    end
                )
            end
        )
    end.
