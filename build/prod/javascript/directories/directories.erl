-module(directories).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([tmp_dir/0, home_dir/0, cache_dir/0, config_dir/0, config_local_dir/0, data_dir/0, data_local_dir/0, executable_dir/0, preference_dir/0, runtime_dir/0, state_dir/0, main/0]).

-spec check_dirs(list(binary())) -> {ok, binary()} | {error, nil}.
check_dirs(Paths) ->
    _pipe = Paths,
    _pipe@1 = gleam@list:filter(
        _pipe,
        fun(A) ->
            not gleam@string:is_empty(A) andalso gleam@result:unwrap(
                simplifile_erl:is_directory(A),
                false
            )
        end
    ),
    gleam@list:first(_pipe@1).

-spec check_dir_from_env(list(binary())) -> {ok, binary()} | {error, nil}.
check_dir_from_env(Vars) ->
    _pipe = Vars,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(_capture) -> envoy_ffi:get(_capture) end
    ),
    check_dirs(_pipe@1).

-spec get_env(binary()) -> binary().
get_env(Var) ->
    gleam@result:unwrap(envoy_ffi:get(Var), <<""/utf8>>).

-spec home_dir_path(binary()) -> binary().
home_dir_path(Path) ->
    <<(get_env(<<"HOME"/utf8>>))/binary, Path/binary>>.

-spec other_os_message(binary()) -> {ok, binary()} | {error, nil}.
other_os_message(Other_os) ->
    gleam@io:print_error(
        <<<<"[WARN][directories] Operating system '"/utf8, Other_os/binary>>/binary,
            "' is not supported by this library"/utf8>>
    ),
    {error, nil}.

-spec tmp_dir() -> {ok, binary()} | {error, nil}.
tmp_dir() ->
    case check_dir_from_env(
        [<<"TMPDIR"/utf8>>, <<"TEMP"/utf8>>, <<"TMP"/utf8>>]
    ) of
        {ok, Path} ->
            {ok, Path};

        {error, nil} ->
            case platform:os() of
                win32 ->
                    check_dirs(
                        [<<"C:\\TEMP"/utf8>>,
                            <<"C:\\TMP"/utf8>>,
                            <<"\\TEMP"/utf8>>,
                            <<"\\TMP"/utf8>>]
                    );

                darwin ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                linux ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                free_bsd ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                open_bsd ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                sun_os ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                aix ->
                    check_dirs(
                        [<<"/tmp"/utf8>>,
                            <<"/var/tmp"/utf8>>,
                            <<"/usr/tmp"/utf8>>]
                    );

                {other_os, Os} ->
                    other_os_message(Os)
            end
    end.

-spec home_dir() -> {ok, binary()} | {error, nil}.
home_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"UserProfile"/utf8>>, <<"Profile"/utf8>>]);

        darwin ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        linux ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        free_bsd ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        open_bsd ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        sun_os ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        aix ->
            check_dir_from_env([<<"HOME"/utf8>>]);

        {other_os, Os} ->
            other_os_message(Os)
    end.

-spec cache_dir() -> {ok, binary()} | {error, nil}.
cache_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"APPDATA"/utf8>>]);

        darwin ->
            check_dirs(
                [<<(get_env(<<"HOME"/utf8>>))/binary, "/Library/Caches"/utf8>>]
            );

        linux ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_CACHE_HOME"/utf8>>),
                    home_dir_path(<<"/.cache"/utf8>>)]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-spec config_dir() -> {ok, binary()} | {error, nil}.
config_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"APPDATA"/utf8>>]);

        darwin ->
            check_dirs(
                [<<(get_env(<<"HOME"/utf8>>))/binary,
                        "/Library/Application Support"/utf8>>]
            );

        linux ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_CONFIG_HOME"/utf8>>),
                    home_dir_path(<<"/.config"/utf8>>)]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-spec config_local_dir() -> {ok, binary()} | {error, nil}.
config_local_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"LOCALAPPDATA"/utf8>>]);

        _ ->
            config_dir()
    end.

-spec data_dir() -> {ok, binary()} | {error, nil}.
data_dir() ->
    case platform:os() of
        linux ->
            check_dirs(
                [get_env(<<"XDG_DATA_HOME"/utf8>>),
                    home_dir_path(<<"/.local/share"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_DATA_HOME"/utf8>>),
                    home_dir_path(<<"/.local/share"/utf8>>)]
            );

        _ ->
            config_dir()
    end.

-spec data_local_dir() -> {ok, binary()} | {error, nil}.
data_local_dir() ->
    case platform:os() of
        win32 ->
            check_dir_from_env([<<"LOCALAPPDATA"/utf8>>]);

        _ ->
            data_dir()
    end.

-spec executable_dir() -> {ok, binary()} | {error, nil}.
executable_dir() ->
    case platform:os() of
        win32 ->
            {error, nil};

        darwin ->
            {error, nil};

        linux ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_BIN_HOME"/utf8>>),
                    home_dir_path(<<"/.local/bin"/utf8>>),
                    <<(get_env(<<"XDG_DATA_HOME"/utf8>>))/binary,
                        "../bin"/utf8>>]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-spec preference_dir() -> {ok, binary()} | {error, nil}.
preference_dir() ->
    case platform:os() of
        darwin ->
            check_dirs([home_dir_path(<<"/Library/Preferences"/utf8>>)]);

        _ ->
            config_dir()
    end.

-spec runtime_dir() -> {ok, binary()} | {error, nil}.
runtime_dir() ->
    case platform:os() of
        win32 ->
            {error, nil};

        darwin ->
            {error, nil};

        linux ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        free_bsd ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        open_bsd ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        sun_os ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        aix ->
            check_dir_from_env([<<"XDG_RUNTIME_DIR"/utf8>>]);

        {other_os, Os} ->
            other_os_message(Os)
    end.

-spec state_dir() -> {ok, binary()} | {error, nil}.
state_dir() ->
    case platform:os() of
        win32 ->
            {error, nil};

        darwin ->
            {error, nil};

        linux ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        free_bsd ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        open_bsd ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        sun_os ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        aix ->
            check_dirs(
                [get_env(<<"XDG_STATE_HOME"/utf8>>),
                    home_dir_path(<<"/.local/state"/utf8>>)]
            );

        {other_os, Os} ->
            other_os_message(Os)
    end.

-spec main() -> {ok, binary()} | {error, nil}.
main() ->
    gleam@io:print(<<"Current Platform: "/utf8>>),
    _ = gleam@io:debug(platform:os()),
    gleam@io:println(<<"==="/utf8>>),
    gleam@io:print(<<"Temp Directory: "/utf8>>),
    _ = gleam@io:debug(tmp_dir()),
    gleam@io:print(<<"Home Directory: "/utf8>>),
    _ = gleam@io:debug(home_dir()),
    gleam@io:print(<<"Cache Directory: "/utf8>>),
    _ = gleam@io:debug(cache_dir()),
    gleam@io:print(<<"Config Directory: "/utf8>>),
    _ = gleam@io:debug(config_dir()),
    gleam@io:print(<<"Config Directory (Local): "/utf8>>),
    _ = gleam@io:debug(config_local_dir()),
    gleam@io:print(<<"Data Directory: "/utf8>>),
    _ = gleam@io:debug(data_dir()),
    gleam@io:print(<<"Data Directory (Local): "/utf8>>),
    _ = gleam@io:debug(data_local_dir()),
    gleam@io:print(<<"Executables Directory: "/utf8>>),
    _ = gleam@io:debug(executable_dir()),
    gleam@io:print(<<"Preferences Directory: "/utf8>>),
    _ = gleam@io:debug(preference_dir()),
    gleam@io:print(<<"Runtime Directory: "/utf8>>),
    _ = gleam@io:debug(runtime_dir()),
    gleam@io:print(<<"State Directory: "/utf8>>),
    _ = gleam@io:debug(state_dir()).
