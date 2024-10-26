-module(lustre_dev_tools@cli@add).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([esbuild/0, tailwind/0]).

-spec esbuild() -> glint:command(nil).
esbuild() ->
    Description = <<"
Download a platform-appropriate version of the esbuild binary. Lustre uses this
to bundle applications and act as a development server, and will automatically
download the binary if either the `build` or `start` commands are run.
    "/utf8>>,
    glint:command_help(
        Description,
        fun() ->
            glint:unnamed_args(
                {eq_args, 0},
                fun() ->
                    glint:flag(
                        lustre_dev_tools@cli@flag:esbuild_os(),
                        fun(Os) ->
                            glint:flag(
                                lustre_dev_tools@cli@flag:esbuild_cpu(),
                                fun(Cpu) ->
                                    glint:command(
                                        fun(_, _, Flags) ->
                                            Script = (lustre_dev_tools@cli:do(
                                                lustre_dev_tools@cli:get_string(
                                                    <<"os"/utf8>>,
                                                    lustre_dev_tools_ffi:get_os(
                                                        
                                                    ),
                                                    [<<"add"/utf8>>],
                                                    Os
                                                ),
                                                fun(Os@1) ->
                                                    lustre_dev_tools@cli:do(
                                                        lustre_dev_tools@cli:get_string(
                                                            <<"cpu"/utf8>>,
                                                            lustre_dev_tools_ffi:get_cpu(
                                                                
                                                            ),
                                                            [<<"add"/utf8>>],
                                                            Cpu
                                                        ),
                                                        fun(Cpu@1) ->
                                                            lustre_dev_tools@esbuild:download(
                                                                Os@1,
                                                                Cpu@1
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

                                                {error, Error} ->
                                                    _pipe = lustre_dev_tools@error:explain(
                                                        Error
                                                    ),
                                                    gleam@io:print_error(_pipe)
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
    ).

-spec tailwind() -> glint:command(nil).
tailwind() ->
    Description = <<"
Download a platform-appropriate version of the Tailwind binary. Lustre will
automatically use this to compile your styles if it detects a `tailwind.config.js`
in your project but will not download it automatically.
    "/utf8>>,
    glint:command_help(
        Description,
        fun() ->
            glint:unnamed_args(
                {eq_args, 0},
                fun() ->
                    glint:flag(
                        lustre_dev_tools@cli@flag:tailwind_os(),
                        fun(Os) ->
                            glint:flag(
                                lustre_dev_tools@cli@flag:tailwind_cpu(),
                                fun(Cpu) ->
                                    glint:command(
                                        fun(_, _, Flags) ->
                                            Script = (lustre_dev_tools@cli:do(
                                                lustre_dev_tools@cli:get_string(
                                                    <<"os"/utf8>>,
                                                    lustre_dev_tools_ffi:get_os(
                                                        
                                                    ),
                                                    [<<"add"/utf8>>],
                                                    Os
                                                ),
                                                fun(Os@1) ->
                                                    lustre_dev_tools@cli:do(
                                                        lustre_dev_tools@cli:get_string(
                                                            <<"cpu"/utf8>>,
                                                            lustre_dev_tools_ffi:get_cpu(
                                                                
                                                            ),
                                                            [<<"add"/utf8>>],
                                                            Cpu
                                                        ),
                                                        fun(Cpu@1) ->
                                                            lustre_dev_tools@tailwind:setup(
                                                                Os@1,
                                                                Cpu@1
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

                                                {error, Error} ->
                                                    _pipe = lustre_dev_tools@error:explain(
                                                        Error
                                                    ),
                                                    gleam@io:print_error(_pipe)
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
    ).
