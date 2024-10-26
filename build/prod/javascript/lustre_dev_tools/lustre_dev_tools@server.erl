-module(lustre_dev_tools@server).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/1]).

-spec inject_live_reload(
    gleam@http@request:request(wisp@internal:connection()),
    binary(),
    fun(() -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
inject_live_reload(Req, Root, K) ->
    _assert_subject = gleam@regex:from_string(<<".*\\.html$"/utf8>>),
    {ok, Is_interesting} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/server"/utf8>>,
                        function => <<"inject_live_reload"/utf8>>,
                        line => 84})
    end,
    gleam@bool:lazy_guard(
        not gleam@regex:check(Is_interesting, erlang:element(8, Req)),
        K,
        fun() ->
            Path = filepath:join(Root, erlang:element(8, Req)),
            case simplifile_erl:is_file(Path) of
                {ok, false} ->
                    K();

                {error, _} ->
                    K();

                {ok, true} ->
                    _assert_subject@1 = simplifile:read(Path),
                    {ok, Html} = case _assert_subject@1 of
                        {ok, _} -> _assert_subject@1;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@1,
                                        module => <<"lustre_dev_tools/server"/utf8>>,
                                        function => <<"inject_live_reload"/utf8>>,
                                        line => 91})
                    end,
                    _pipe = Html,
                    _pipe@1 = lustre_dev_tools@server@live_reload:inject(_pipe),
                    _pipe@2 = gleam@string_builder:from_string(_pipe@1),
                    wisp:html_response(_pipe@2, 200)
            end
        end
    ).

-spec handler(gleam@http@request:request(wisp@internal:connection()), binary()) -> gleam@http@response:response(wisp:body()).
handler(Req, Root) ->
    inject_live_reload(
        Req,
        Root,
        fun() ->
            wisp:serve_static(
                Req,
                <<"/"/utf8>>,
                Root,
                fun() ->
                    handler(
                        erlang:setelement(8, Req, <<"/index.html"/utf8>>),
                        Root
                    )
                end
            )
        end
    ).

-spec start(integer()) -> lustre_dev_tools@cli:cli(nil).
start(Port) ->
    _assert_subject = lustre_dev_tools_ffi:get_cwd(),
    {ok, Cwd} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/server"/utf8>>,
                        function => <<"start"/utf8>>,
                        line => 25})
    end,
    _assert_subject@1 = filepath:expand(
        filepath:join(Cwd, lustre_dev_tools@project:root())
    ),
    {ok, Root} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre_dev_tools/server"/utf8>>,
                        function => <<"start"/utf8>>,
                        line => 26})
    end,
    lustre_dev_tools@cli:do(
        lustre_dev_tools@server@proxy:get(),
        fun(Proxy) ->
            case Proxy of
                {some, _} ->
                    gleam@io:println(
                        <<"
[WARNING] Support for proxying requests to another server is currently still
**experimental**. It's functionality or api may change is breaking ways even
between minor versions. If you run into any problems please open an issue over
at https://github.com/lustre-labs/dev-tools/issues/new
      "/utf8>>
                    );

                none ->
                    nil
            end,
            lustre_dev_tools@cli:do(
                lustre_dev_tools@cli:get_flags(),
                fun(Flags) ->
                    lustre_dev_tools@cli:'try'(
                        lustre_dev_tools@server@live_reload:start(Root, Flags),
                        fun(Make_socket) ->
                            lustre_dev_tools@cli:'try'(
                                begin
                                    _pipe@1 = fun(Req) ->
                                        lustre_dev_tools@server@proxy:middleware(
                                            Req,
                                            Proxy,
                                            fun() ->
                                                case gleam@http@request:path_segments(
                                                    Req
                                                ) of
                                                    [<<"lustre-dev-tools"/utf8>>] ->
                                                        Make_socket(Req);

                                                    [] ->
                                                        _pipe = erlang:setelement(
                                                            8,
                                                            Req,
                                                            <<"/index.html"/utf8>>
                                                        ),
                                                        (wisp@wisp_mist:handler(
                                                            fun(_capture) ->
                                                                handler(
                                                                    _capture,
                                                                    Root
                                                                )
                                                            end,
                                                            <<""/utf8>>
                                                        ))(_pipe);

                                                    _ ->
                                                        (wisp@wisp_mist:handler(
                                                            fun(_capture@1) ->
                                                                handler(
                                                                    _capture@1,
                                                                    Root
                                                                )
                                                            end,
                                                            <<""/utf8>>
                                                        ))(Req)
                                                end
                                            end
                                        )
                                    end,
                                    _pipe@2 = mist:new(_pipe@1),
                                    _pipe@3 = mist:port(_pipe@2, Port),
                                    _pipe@4 = mist:start_http(_pipe@3),
                                    gleam@result:map_error(
                                        _pipe@4,
                                        fun(Field@0) -> {cannot_start_dev_server, Field@0} end
                                    )
                                end,
                                fun(_) ->
                                    lustre_dev_tools@cli:return(
                                        gleam_erlang_ffi:sleep_forever()
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
