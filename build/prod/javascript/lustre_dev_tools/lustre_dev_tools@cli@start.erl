-module(lustre_dev_tools@cli@start).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([run/0]).

-spec check_otp_version() -> lustre_dev_tools@cli:cli(nil).
check_otp_version() ->
    lustre_dev_tools@cli:log(
        <<"Checking OTP version"/utf8>>,
        fun() ->
            Version = lustre_dev_tools@project:otp_version(),
            case Version =< 25 of
                false ->
                    lustre_dev_tools@cli:return(nil);

                true ->
                    lustre_dev_tools@cli:throw({otp_too_old, Version})
            end
        end
    ).

-spec write_html(binary(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
write_html(Path, Source) ->
    _pipe = simplifile:write(Path, Source),
    gleam@result:map_error(
        _pipe,
        fun(_capture) -> {cannot_write_file, _capture, Path} end
    ).

-spec prepare_html() -> lustre_dev_tools@cli:cli(nil).
prepare_html() ->
    _assert_subject = lustre_dev_tools_ffi:get_cwd(),
    {ok, Cwd} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/cli/start"/utf8>>,
                        function => <<"prepare_html"/utf8>>,
                        line => 74})
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
                        module => <<"lustre_dev_tools/cli/start"/utf8>>,
                        function => <<"prepare_html"/utf8>>,
                        line => 75})
    end,
    Index = filepath:join(Root, <<"index.html"/utf8>>),
    case simplifile_erl:is_file(Index) of
        {ok, true} ->
            lustre_dev_tools@cli:return(nil);

        {ok, false} ->
            lustre_dev_tools@cli:template(
                <<"index.html"/utf8>>,
                fun(Html) ->
                    lustre_dev_tools@cli:do(
                        lustre_dev_tools@cli:get_name(),
                        fun(App_name) ->
                            Html@1 = gleam@string:replace(
                                Html,
                                <<"{app_name}"/utf8>>,
                                App_name
                            ),
                            lustre_dev_tools@cli:'try'(
                                write_html(Index, Html@1),
                                fun(_) -> lustre_dev_tools@cli:return(nil) end
                            )
                        end
                    )
                end
            );

        {error, _} ->
            lustre_dev_tools@cli:template(
                <<"index.html"/utf8>>,
                fun(Html) ->
                    lustre_dev_tools@cli:do(
                        lustre_dev_tools@cli:get_name(),
                        fun(App_name) ->
                            Html@1 = gleam@string:replace(
                                Html,
                                <<"{app_name}"/utf8>>,
                                App_name
                            ),
                            lustre_dev_tools@cli:'try'(
                                write_html(Index, Html@1),
                                fun(_) -> lustre_dev_tools@cli:return(nil) end
                            )
                        end
                    )
                end
            )
    end.

-spec run() -> glint:command(nil).
run() ->
    Description = <<"
Start a development server for your Lustre project. This command will compile your
application and serve it on a local server. If your application's `main` function
returns a compatible `App`, this will generate the necessary code to start it.
Otherwise, your `main` function will be used as the entry point.


This development server does *not* currently watch your files for changes.
Watchexec is a popular tool you can use to restart the server when files change.
    "/utf8>>,
    glint:command_help(
        Description,
        fun() ->
            glint:unnamed_args(
                {eq_args, 0},
                fun() ->
                    glint:flag(
                        lustre_dev_tools@cli@flag:port(),
                        fun(Port) ->
                            glint:flag(
                                lustre_dev_tools@cli@flag:proxy_from(),
                                fun(_) ->
                                    glint:flag(
                                        lustre_dev_tools@cli@flag:proxy_to(),
                                        fun(_) ->
                                            glint:flag(
                                                lustre_dev_tools@cli@flag:detect_tailwind(
                                                    
                                                ),
                                                fun(Detect_tailwind) ->
                                                    glint:flag(
                                                        lustre_dev_tools@cli@flag:tailwind_entry(
                                                            
                                                        ),
                                                        fun(_) ->
                                                            glint:command(
                                                                fun(_, _, Flags) ->
                                                                    Script = (lustre_dev_tools@cli:do(
                                                                        lustre_dev_tools@cli:get_int(
                                                                            <<"port"/utf8>>,
                                                                            1234,
                                                                            [<<"start"/utf8>>],
                                                                            Port
                                                                        ),
                                                                        fun(
                                                                            Port@1
                                                                        ) ->
                                                                            lustre_dev_tools@cli:do(
                                                                                lustre_dev_tools@cli:get_bool(
                                                                                    <<"detect_tailwind"/utf8>>,
                                                                                    true,
                                                                                    [<<"build"/utf8>>],
                                                                                    Detect_tailwind
                                                                                ),
                                                                                fun(
                                                                                    Detect_tailwind@1
                                                                                ) ->
                                                                                    lustre_dev_tools@cli:do(
                                                                                        check_otp_version(
                                                                                            
                                                                                        ),
                                                                                        fun(
                                                                                            _
                                                                                        ) ->
                                                                                            lustre_dev_tools@cli:do(
                                                                                                lustre_dev_tools@cli@build:do_app(
                                                                                                    false,
                                                                                                    Detect_tailwind@1
                                                                                                ),
                                                                                                fun(
                                                                                                    _
                                                                                                ) ->
                                                                                                    lustre_dev_tools@cli:do(
                                                                                                        prepare_html(
                                                                                                            
                                                                                                        ),
                                                                                                        fun(
                                                                                                            _
                                                                                                        ) ->
                                                                                                            lustre_dev_tools@cli:do(
                                                                                                                lustre_dev_tools@server:start(
                                                                                                                    Port@1
                                                                                                                ),
                                                                                                                fun(
                                                                                                                    _
                                                                                                                ) ->
                                                                                                                    lustre_dev_tools@cli:return(
                                                                                                                        nil
                                                                                                                    )
                                                                                                                end
                                                                                                            )
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            )
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )),
                                                                    case lustre_dev_tools@cli:run(
                                                                        Script,
                                                                        Flags
                                                                    ) of
                                                                        {ok, _} ->
                                                                            nil;

                                                                        {error,
                                                                            Error} ->
                                                                            _pipe = lustre_dev_tools@error:explain(
                                                                                Error
                                                                            ),
                                                                            gleam@io:print_error(
                                                                                _pipe
                                                                            )
                                                                    end
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
