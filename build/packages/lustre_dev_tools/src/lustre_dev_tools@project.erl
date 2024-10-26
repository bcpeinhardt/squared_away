-module(lustre_dev_tools@project).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([otp_version/0, build/0, root/0, config/0, type_to_string/1, interface/0]).
-export_type([config/0, interface/0, module_/0, function_/0]).

-type config() :: {config,
        binary(),
        binary(),
        gleam@dict:dict(binary(), tom:toml())}.

-type interface() :: {interface,
        binary(),
        binary(),
        gleam@dict:dict(binary(), module_())}.

-type module_() :: {module,
        gleam@dict:dict(binary(), gleam@package_interface:type()),
        gleam@dict:dict(binary(), function_())}.

-type function_() :: {function,
        list(gleam@package_interface:type()),
        gleam@package_interface:type()}.

-spec otp_version() -> integer().
otp_version() ->
    Version = lustre_dev_tools_ffi:otp_version(),
    case gleam@int:parse(Version) of
        {ok, Version@1} ->
            Version@1;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"unexpected version number format: "/utf8,
                        Version/binary>>),
                    module => <<"lustre_dev_tools/project"/utf8>>,
                    function => <<"otp_version"/utf8>>,
                    line => 42})
    end.

-spec build() -> {ok, nil} | {error, lustre_dev_tools@error:error()}.
build() ->
    _pipe = lustre_dev_tools_ffi:exec(
        <<"gleam"/utf8>>,
        [<<"build"/utf8>>, <<"--target"/utf8>>, <<"javascript"/utf8>>],
        <<"."/utf8>>
    ),
    _pipe@1 = gleam@result:map_error(
        _pipe,
        fun(Err) -> {build_error, gleam@pair:second(Err)} end
    ),
    gleam@result:replace(_pipe@1, nil).

-spec find_root(binary()) -> binary().
find_root(Path) ->
    Toml = filepath:join(Path, <<"gleam.toml"/utf8>>),
    case simplifile_erl:is_file(Toml) of
        {ok, false} ->
            find_root(filepath:join(<<".."/utf8>>, Path));

        {error, _} ->
            find_root(filepath:join(<<".."/utf8>>, Path));

        {ok, true} ->
            Path
    end.

-spec root() -> binary().
root() ->
    find_root(<<"."/utf8>>).

-spec config() -> {ok, config()} | {error, lustre_dev_tools@error:error()}.
config() ->
    Configuration_path = filepath:join(root(), <<"gleam.toml"/utf8>>),
    _assert_subject = simplifile:read(Configuration_path),
    {ok, Configuration} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 105})
    end,
    _assert_subject@1 = tom:parse(Configuration),
    {ok, Toml} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 106})
    end,
    _assert_subject@2 = tom:get_string(Toml, [<<"name"/utf8>>]),
    {ok, Name} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 107})
    end,
    _assert_subject@3 = tom:get_string(Toml, [<<"version"/utf8>>]),
    {ok, Version} = case _assert_subject@3 of
        {ok, _} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@3,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 108})
    end,
    {ok, {config, Name, Version, Toml}}.

-spec type_to_string(gleam@package_interface:type()) -> binary().
type_to_string(Type_) ->
    case Type_ of
        {tuple, Elements} ->
            Elements@1 = gleam@list:map(Elements, fun type_to_string/1),
            <<<<"#("/utf8,
                    (gleam@string:join(Elements@1, <<", "/utf8>>))/binary>>/binary,
                ")"/utf8>>;

        {fn, Params, Return} ->
            Params@1 = gleam@list:map(Params, fun type_to_string/1),
            Return@1 = type_to_string(Return),
            <<<<<<"fn("/utf8,
                        (gleam@string:join(Params@1, <<", "/utf8>>))/binary>>/binary,
                    ") -> "/utf8>>/binary,
                Return@1/binary>>;

        {named, Name, _, _, []} ->
            Name;

        {named, Name@1, _, _, Params@2} ->
            Params@3 = gleam@list:map(Params@2, fun type_to_string/1),
            <<<<<<Name@1/binary, "("/utf8>>/binary,
                    (gleam@string:join(Params@3, <<", "/utf8>>))/binary>>/binary,
                ")"/utf8>>;

        {variable, Id} ->
            <<"a_"/utf8, (gleam@int:to_string(Id))/binary>>
    end.

-spec labelled_argument_decoder(gleam@dynamic:dynamic_()) -> {ok,
        gleam@package_interface:type()} |
    {error, list(gleam@dynamic:decode_error())}.
labelled_argument_decoder(Dyn) ->
    (gleam@dynamic:field(
        <<"type"/utf8>>,
        fun gleam@package_interface:type_decoder/1
    ))(Dyn).

-spec function_decoder(gleam@dynamic:dynamic_()) -> {ok, function_()} |
    {error, list(gleam@dynamic:decode_error())}.
function_decoder(Dyn) ->
    (gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {function, Field@0, Field@1} end,
        gleam@dynamic:field(
            <<"parameters"/utf8>>,
            gleam@dynamic:list(fun labelled_argument_decoder/1)
        ),
        gleam@dynamic:field(
            <<"return"/utf8>>,
            fun gleam@package_interface:type_decoder/1
        )
    ))(Dyn).

-spec string_dict(
    fun((gleam@dynamic:dynamic_()) -> {ok, XIP} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dict:dict(binary(), XIP)} |
    {error, list(gleam@dynamic:decode_error())}).
string_dict(Values) ->
    gleam@dynamic:dict(fun gleam@dynamic:string/1, Values).

-spec module_decoder(gleam@dynamic:dynamic_()) -> {ok, module_()} |
    {error, list(gleam@dynamic:decode_error())}.
module_decoder(Dyn) ->
    (gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {module, Field@0, Field@1} end,
        gleam@dynamic:field(
            <<"constants"/utf8>>,
            string_dict(
                gleam@dynamic:field(
                    <<"type"/utf8>>,
                    fun gleam@package_interface:type_decoder/1
                )
            )
        ),
        gleam@dynamic:field(
            <<"functions"/utf8>>,
            string_dict(fun function_decoder/1)
        )
    ))(Dyn).

-spec interface_decoder(gleam@dynamic:dynamic_()) -> {ok, interface()} |
    {error, list(gleam@dynamic:decode_error())}.
interface_decoder(Dyn) ->
    (gleam@dynamic:decode3(
        fun(Field@0, Field@1, Field@2) -> {interface, Field@0, Field@1, Field@2} end,
        gleam@dynamic:field(<<"name"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"version"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"modules"/utf8>>,
            string_dict(fun module_decoder/1)
        )
    ))(Dyn).

-spec interface() -> {ok, interface()} | {error, lustre_dev_tools@error:error()}.
interface() ->
    gleam@result:'try'(
        config(),
        fun(_use0) ->
            {config, Name, _, _} = _use0,
            Caches = [<<"build/prod/javascript"/utf8>>,
                <<"build/prod/erlang"/utf8>>,
                <<"build/dev/javascript"/utf8>>,
                <<"build/dev/erlang"/utf8>>],
            gleam@list:each(
                Caches,
                fun(Cache) -> _pipe = filepath:join(root(), Cache),
                    _pipe@1 = filepath:join(_pipe, Name),
                    simplifile_erl:delete(_pipe@1) end
            ),
            Dir = filepath:join(root(), <<"build/.lustre"/utf8>>),
            Out = filepath:join(Dir, <<"package-interface.json"/utf8>>),
            Args = [<<"export"/utf8>>,
                <<"package-interface"/utf8>>,
                <<"--out"/utf8>>,
                Out],
            _pipe@2 = lustre_dev_tools_ffi:exec(
                <<"gleam"/utf8>>,
                Args,
                <<"."/utf8>>
            ),
            _pipe@3 = gleam@result:map_error(
                _pipe@2,
                fun(Err) -> {build_error, gleam@pair:second(Err)} end
            ),
            gleam@result:then(
                _pipe@3,
                fun(_) ->
                    _assert_subject = simplifile:read(Out),
                    {ok, Json} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail,
                                        module => <<"lustre_dev_tools/project"/utf8>>,
                                        function => <<"interface"/utf8>>,
                                        line => 87})
                    end,
                    _assert_subject@1 = gleam@json:decode(
                        Json,
                        fun interface_decoder/1
                    ),
                    {ok, Interface} = case _assert_subject@1 of
                        {ok, _} -> _assert_subject@1;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@1,
                                        module => <<"lustre_dev_tools/project"/utf8>>,
                                        function => <<"interface"/utf8>>,
                                        line => 88})
                    end,
                    {ok, Interface}
                end
            )
        end
    ).
