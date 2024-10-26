-module(gleam@package_interface).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([deprecation_decoder/1, implementations_decoder/1, type_decoder/1, type_alias_decoder/1, constant_decoder/1, parameter_decoder/1, function_decoder/1, constructor_decoder/1, type_definition_decoder/1, module_decoder/1, decoder/1]).
-export_type([package/0, module_/0, type_alias/0, type_definition/0, type_constructor/0, parameter/0, constant/0, function_/0, deprecation/0, implementations/0, type/0]).

-type package() :: {package,
        binary(),
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), module_())}.

-type module_() :: {module,
        list(binary()),
        gleam@dict:dict(binary(), type_alias()),
        gleam@dict:dict(binary(), type_definition()),
        gleam@dict:dict(binary(), constant()),
        gleam@dict:dict(binary(), function_())}.

-type type_alias() :: {type_alias,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        integer(),
        type()}.

-type type_definition() :: {type_definition,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        integer(),
        list(type_constructor())}.

-type type_constructor() :: {type_constructor,
        gleam@option:option(binary()),
        binary(),
        list(parameter())}.

-type parameter() :: {parameter, gleam@option:option(binary()), type()}.

-type constant() :: {constant,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        implementations(),
        type()}.

-type function_() :: {function,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        implementations(),
        list(parameter()),
        type()}.

-type deprecation() :: {deprecation, binary()}.

-type implementations() :: {implementations, boolean(), boolean(), boolean()}.

-type type() :: {tuple, list(type())} |
    {fn, list(type()), type()} |
    {variable, integer()} |
    {named, binary(), binary(), binary(), list(type())}.

-spec deprecation_decoder(gleam@dynamic:dynamic_()) -> {ok, deprecation()} |
    {error, list(gleam@dynamic:decode_error())}.
deprecation_decoder(Dynamic) ->
    (gleam@dynamic:decode1(
        fun(Field@0) -> {deprecation, Field@0} end,
        gleam@dynamic:field(<<"message"/utf8>>, fun gleam@dynamic:string/1)
    ))(Dynamic).

-spec implementations_decoder(gleam@dynamic:dynamic_()) -> {ok,
        implementations()} |
    {error, list(gleam@dynamic:decode_error())}.
implementations_decoder(Dynamic) ->
    (gleam@dynamic:decode3(
        fun(Field@0, Field@1, Field@2) -> {implementations, Field@0, Field@1, Field@2} end,
        gleam@dynamic:field(<<"gleam"/utf8>>, fun gleam@dynamic:bool/1),
        gleam@dynamic:field(
            <<"uses-erlang-externals"/utf8>>,
            fun gleam@dynamic:bool/1
        ),
        gleam@dynamic:field(
            <<"uses-javascript-externals"/utf8>>,
            fun gleam@dynamic:bool/1
        )
    ))(Dynamic).

-spec type_decoder(gleam@dynamic:dynamic_()) -> {ok, type()} |
    {error, list(gleam@dynamic:decode_error())}.
type_decoder(Dynamic) ->
    gleam@result:'try'(
        (gleam@dynamic:field(<<"kind"/utf8>>, fun gleam@dynamic:string/1))(
            Dynamic
        ),
        fun(Kind) -> case Kind of
                <<"variable"/utf8>> ->
                    (gleam@dynamic:decode1(
                        fun(Field@0) -> {variable, Field@0} end,
                        gleam@dynamic:field(
                            <<"id"/utf8>>,
                            fun gleam@dynamic:int/1
                        )
                    ))(Dynamic);

                <<"tuple"/utf8>> ->
                    (gleam@dynamic:decode1(
                        fun(Field@0) -> {tuple, Field@0} end,
                        gleam@dynamic:field(
                            <<"elements"/utf8>>,
                            gleam@dynamic:list(fun type_decoder/1)
                        )
                    ))(Dynamic);

                <<"named"/utf8>> ->
                    (gleam@dynamic:decode4(
                        fun(Field@0, Field@1, Field@2, Field@3) -> {named, Field@0, Field@1, Field@2, Field@3} end,
                        gleam@dynamic:field(
                            <<"name"/utf8>>,
                            fun gleam@dynamic:string/1
                        ),
                        gleam@dynamic:field(
                            <<"package"/utf8>>,
                            fun gleam@dynamic:string/1
                        ),
                        gleam@dynamic:field(
                            <<"module"/utf8>>,
                            fun gleam@dynamic:string/1
                        ),
                        gleam@dynamic:field(
                            <<"parameters"/utf8>>,
                            gleam@dynamic:list(fun type_decoder/1)
                        )
                    ))(Dynamic);

                <<"fn"/utf8>> ->
                    (gleam@dynamic:decode2(
                        fun(Field@0, Field@1) -> {fn, Field@0, Field@1} end,
                        gleam@dynamic:field(
                            <<"parameters"/utf8>>,
                            gleam@dynamic:list(fun type_decoder/1)
                        ),
                        gleam@dynamic:field(
                            <<"return"/utf8>>,
                            fun type_decoder/1
                        )
                    ))(Dynamic);

                Unknown_tag ->
                    {error,
                        [{decode_error,
                                <<"one of variable, tuple, named, fn"/utf8>>,
                                Unknown_tag,
                                [<<"kind"/utf8>>]}]}
            end end
    ).

-spec type_alias_decoder(gleam@dynamic:dynamic_()) -> {ok, type_alias()} |
    {error, list(gleam@dynamic:decode_error())}.
type_alias_decoder(Dynamic) ->
    (gleam@dynamic:decode4(
        fun(Field@0, Field@1, Field@2, Field@3) -> {type_alias, Field@0, Field@1, Field@2, Field@3} end,
        gleam@dynamic:field(
            <<"documentation"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"deprecation"/utf8>>,
            gleam@dynamic:optional(fun deprecation_decoder/1)
        ),
        gleam@dynamic:field(<<"parameters"/utf8>>, fun gleam@dynamic:int/1),
        gleam@dynamic:field(<<"alias"/utf8>>, fun type_decoder/1)
    ))(Dynamic).

-spec constant_decoder(gleam@dynamic:dynamic_()) -> {ok, constant()} |
    {error, list(gleam@dynamic:decode_error())}.
constant_decoder(Dynamic) ->
    (gleam@dynamic:decode4(
        fun(Field@0, Field@1, Field@2, Field@3) -> {constant, Field@0, Field@1, Field@2, Field@3} end,
        gleam@dynamic:field(
            <<"documentation"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"deprecation"/utf8>>,
            gleam@dynamic:optional(fun deprecation_decoder/1)
        ),
        gleam@dynamic:field(
            <<"implementations"/utf8>>,
            fun implementations_decoder/1
        ),
        gleam@dynamic:field(<<"type"/utf8>>, fun type_decoder/1)
    ))(Dynamic).

-spec parameter_decoder(gleam@dynamic:dynamic_()) -> {ok, parameter()} |
    {error, list(gleam@dynamic:decode_error())}.
parameter_decoder(Dynamic) ->
    (gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {parameter, Field@0, Field@1} end,
        gleam@dynamic:field(
            <<"label"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(<<"type"/utf8>>, fun type_decoder/1)
    ))(Dynamic).

-spec function_decoder(gleam@dynamic:dynamic_()) -> {ok, function_()} |
    {error, list(gleam@dynamic:decode_error())}.
function_decoder(Dynamic) ->
    (gleam@dynamic:decode5(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4) -> {function, Field@0, Field@1, Field@2, Field@3, Field@4} end,
        gleam@dynamic:field(
            <<"documentation"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"deprecation"/utf8>>,
            gleam@dynamic:optional(fun deprecation_decoder/1)
        ),
        gleam@dynamic:field(
            <<"implementations"/utf8>>,
            fun implementations_decoder/1
        ),
        gleam@dynamic:field(
            <<"parameters"/utf8>>,
            gleam@dynamic:list(fun parameter_decoder/1)
        ),
        gleam@dynamic:field(<<"return"/utf8>>, fun type_decoder/1)
    ))(Dynamic).

-spec constructor_decoder(gleam@dynamic:dynamic_()) -> {ok, type_constructor()} |
    {error, list(gleam@dynamic:decode_error())}.
constructor_decoder(Dynamic) ->
    (gleam@dynamic:decode3(
        fun(Field@0, Field@1, Field@2) -> {type_constructor, Field@0, Field@1, Field@2} end,
        gleam@dynamic:field(
            <<"documentation"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"parameters"/utf8>>,
            gleam@dynamic:list(fun parameter_decoder/1)
        )
    ))(Dynamic).

-spec type_definition_decoder(gleam@dynamic:dynamic_()) -> {ok,
        type_definition()} |
    {error, list(gleam@dynamic:decode_error())}.
type_definition_decoder(Dynamic) ->
    (gleam@dynamic:decode4(
        fun(Field@0, Field@1, Field@2, Field@3) -> {type_definition, Field@0, Field@1, Field@2, Field@3} end,
        gleam@dynamic:field(
            <<"documentation"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"deprecation"/utf8>>,
            gleam@dynamic:optional(fun deprecation_decoder/1)
        ),
        gleam@dynamic:field(<<"parameters"/utf8>>, fun gleam@dynamic:int/1),
        gleam@dynamic:field(
            <<"constructors"/utf8>>,
            gleam@dynamic:list(fun constructor_decoder/1)
        )
    ))(Dynamic).

-spec string_dict(
    fun((gleam@dynamic:dynamic_()) -> {ok, MHM} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dict:dict(binary(), MHM)} |
    {error, list(gleam@dynamic:decode_error())}).
string_dict(Values) ->
    gleam@dynamic:dict(fun gleam@dynamic:string/1, Values).

-spec module_decoder(gleam@dynamic:dynamic_()) -> {ok, module_()} |
    {error, list(gleam@dynamic:decode_error())}.
module_decoder(Dynamic) ->
    (gleam@dynamic:decode5(
        fun(Field@0, Field@1, Field@2, Field@3, Field@4) -> {module, Field@0, Field@1, Field@2, Field@3, Field@4} end,
        gleam@dynamic:field(
            <<"documentation"/utf8>>,
            gleam@dynamic:list(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"type-aliases"/utf8>>,
            string_dict(fun type_alias_decoder/1)
        ),
        gleam@dynamic:field(
            <<"types"/utf8>>,
            string_dict(fun type_definition_decoder/1)
        ),
        gleam@dynamic:field(
            <<"constants"/utf8>>,
            string_dict(fun constant_decoder/1)
        ),
        gleam@dynamic:field(
            <<"functions"/utf8>>,
            string_dict(fun function_decoder/1)
        )
    ))(Dynamic).

-spec decoder(gleam@dynamic:dynamic_()) -> {ok, package()} |
    {error, list(gleam@dynamic:decode_error())}.
decoder(Dynamic) ->
    (gleam@dynamic:decode4(
        fun(Field@0, Field@1, Field@2, Field@3) -> {package, Field@0, Field@1, Field@2, Field@3} end,
        gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"version"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"gleam-version-constraint"/utf8>>,
            gleam@dynamic:optional(fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"modules"/utf8>>,
            string_dict(fun module_decoder/1)
        )
    ))(Dynamic).
