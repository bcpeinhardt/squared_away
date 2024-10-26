-module(lustre_dev_tools@tailwind).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([setup/2]).

-spec display_next_steps() -> lustre_dev_tools@cli:cli(nil).
display_next_steps() ->
    lustre_dev_tools@cli:notify(
        gleam_community@ansi:bold(<<"\nNext Steps:\n"/utf8>>),
        fun() ->
            lustre_dev_tools@cli:notify(
                <<"1. Be sure to update your root `index.html` file to include \n   `<link rel='stylesheet' type='text/css' href='./priv/static/your_app.css' />`"/utf8>>,
                fun() -> lustre_dev_tools@cli:return(nil) end
            )
        end
    ).

-spec check_tailwind_exists(binary()) -> boolean().
check_tailwind_exists(Path) ->
    case simplifile_erl:is_file(Path) of
        {ok, true} ->
            true;

        {ok, false} ->
            false;

        {error, _} ->
            false
    end.

-spec get_download_url_and_hash(binary(), binary()) -> {ok,
        {binary(), binary()}} |
    {error, lustre_dev_tools@error:error()}.
get_download_url_and_hash(Os, Cpu) ->
    Base = <<"https://github.com/tailwindlabs/tailwindcss/releases/download/v3.4.1/tailwindcss-"/utf8>>,
    case {Os, Cpu} of
        {<<"linux"/utf8>>, <<"armv7"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-armv7"/utf8>>,
                    <<"38E004B144004495CD148621ADB852C21D5D350E66308C8FF9E2FD90A15726F5"/utf8>>}};

        {<<"linux"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm64"/utf8>>,
                    <<"1178C3E8B44B9EB43F40E786EE25664C93D83F6D05B062C0D9CAF410D64D5587"/utf8>>}};

        {<<"linux"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm64"/utf8>>,
                    <<"1178C3E8B44B9EB43F40E786EE25664C93D83F6D05B062C0D9CAF410D64D5587"/utf8>>}};

        {<<"linux"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-x64"/utf8>>,
                    <<"A6814CC8FB6E573DD637352093F3B8E927C5C8628B1FF87826652935AF1430B1"/utf8>>}};

        {<<"linux"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-x64"/utf8>>,
                    <<"A6814CC8FB6E573DD637352093F3B8E927C5C8628B1FF87826652935AF1430B1"/utf8>>}};

        {<<"win32"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "windows-arm64.exe"/utf8>>,
                    <<"AC06BC274FAED7A0C9C0C7E9058D87A428B574BCF8FCE85330F576DE4568BB81"/utf8>>}};

        {<<"win32"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "windows-x64.exe"/utf8>>,
                    <<"EF2FE367DAA8204CB186796C1833FAEE81A5B20E4C80E533F7A3B3DCC7DB6C54"/utf8>>}};

        {<<"win32"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "windows-x64.exe"/utf8>>,
                    <<"EF2FE367DAA8204CB186796C1833FAEE81A5B20E4C80E533F7A3B3DCC7DB6C54"/utf8>>}};

        {<<"darwin"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "macos-arm64"/utf8>>,
                    <<"40738E59ECEF06F955243154E7D1C6EAF11370037CBEEE4A32C3138387E2DA5D"/utf8>>}};

        {<<"darwin"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary, "macos-arm64"/utf8>>,
                    <<"40738E59ECEF06F955243154E7D1C6EAF11370037CBEEE4A32C3138387E2DA5D"/utf8>>}};

        {<<"darwin"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "macos-x64"/utf8>>,
                    <<"594D01B032125199DB105C661FE23DE4C069006921B96F7FEE98EE4FBC15F800"/utf8>>}};

        {<<"darwin"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "macos-x64"/utf8>>,
                    <<"594D01B032125199DB105C661FE23DE4C069006921B96F7FEE98EE4FBC15F800"/utf8>>}};

        {_, _} ->
            {error, {unknown_platform, <<"tailwind"/utf8>>, Os, Cpu}}
    end.

-spec check_tailwind_integrity(bitstring(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
check_tailwind_integrity(Bin, Expected_hash) ->
    Hash = gleam_crypto_ffi:hash(sha256, Bin),
    Hash_string = binary:encode_hex(Hash),
    case Hash_string =:= Expected_hash of
        true ->
            {ok, nil};

        false ->
            {error, invalid_tailwind_binary}
    end.

-spec write_tailwind(bitstring(), binary(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
write_tailwind(Bin, Outdir, Outfile) ->
    _ = simplifile:create_directory_all(Outdir),
    _pipe = simplifile_erl:write_bits(Outfile, Bin),
    gleam@result:map_error(
        _pipe,
        fun(_capture) ->
            {cannot_write_file, _capture, filepath:join(Outdir, Outfile)}
        end
    ).

-spec set_file_permissions(binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
set_file_permissions(File) ->
    Permissions = {file_permissions,
        gleam@set:from_list([read, write, execute]),
        gleam@set:from_list([read, execute]),
        gleam@set:from_list([read, execute])},
    _pipe = simplifile:set_permissions(File, Permissions),
    gleam@result:map_error(
        _pipe,
        fun(_capture) -> {cannot_set_permissions, _capture, File} end
    ).

-spec write_config(binary(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
write_config(Path, Content) ->
    _pipe = simplifile:write(Path, Content),
    gleam@result:map_error(
        _pipe,
        fun(_capture) -> {cannot_write_file, _capture, Path} end
    ).

-spec write_tailwind_config() -> lustre_dev_tools@cli:cli(nil).
write_tailwind_config() ->
    Config_filename = <<"tailwind.config.js"/utf8>>,
    Config_outfile = filepath:join(
        lustre_dev_tools@project:root(),
        Config_filename
    ),
    Config_already_exists = begin
        _pipe = simplifile_erl:is_file(Config_outfile),
        gleam@result:unwrap(_pipe, false)
    end,
    gleam@bool:guard(
        Config_already_exists,
        lustre_dev_tools@cli:return(nil),
        fun() ->
            lustre_dev_tools@cli:log(
                <<<<"Writing `"/utf8, Config_filename/binary>>/binary,
                    "`"/utf8>>,
                fun() ->
                    lustre_dev_tools@cli:template(
                        <<"tailwind.config.js"/utf8>>,
                        fun(Config) ->
                            lustre_dev_tools@cli:'try'(
                                write_config(Config_outfile, Config),
                                fun(_) ->
                                    lustre_dev_tools@cli:success(
                                        <<<<"Written `"/utf8,
                                                Config_outfile/binary>>/binary,
                                            "`"/utf8>>,
                                        fun() ->
                                            lustre_dev_tools@cli:return(nil)
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

-spec get_tailwind(binary()) -> {ok, bitstring()} |
    {error, lustre_dev_tools@error:error()}.
get_tailwind(Url) ->
    _pipe = lustre_dev_tools_ffi:get_esbuild(Url),
    gleam@result:map_error(_pipe, fun(Field@0) -> {network_error, Field@0} end).

-spec download(binary(), binary()) -> lustre_dev_tools@cli:cli(nil).
download(Os, Cpu) ->
    lustre_dev_tools@cli:log(
        <<"Downloading Tailwind"/utf8>>,
        fun() ->
            Root = lustre_dev_tools@project:root(),
            Outdir = filepath:join(Root, <<"build/.lustre/bin"/utf8>>),
            Outfile = filepath:join(Outdir, <<"tailwind"/utf8>>),
            case check_tailwind_exists(Outfile) of
                true ->
                    lustre_dev_tools@cli:success(
                        <<"Tailwind already installed!"/utf8>>,
                        fun() -> lustre_dev_tools@cli:return(nil) end
                    );

                false ->
                    lustre_dev_tools@cli:log(
                        <<"Detecting platform"/utf8>>,
                        fun() ->
                            lustre_dev_tools@cli:'try'(
                                get_download_url_and_hash(Os, Cpu),
                                fun(_use0) ->
                                    {Url, Hash} = _use0,
                                    Max_url_size = lustre_dev_tools@utils:term_width(
                                        
                                    )
                                    - 19,
                                    Shortened_url = lustre_dev_tools@utils:shorten_url(
                                        Url,
                                        Max_url_size
                                    ),
                                    lustre_dev_tools@cli:log(
                                        <<"Downloading from "/utf8,
                                            Shortened_url/binary>>,
                                        fun() ->
                                            lustre_dev_tools@cli:'try'(
                                                get_tailwind(Url),
                                                fun(Bin) ->
                                                    lustre_dev_tools@cli:log(
                                                        <<"Checking the downloaded Tailwind binary"/utf8>>,
                                                        fun() ->
                                                            lustre_dev_tools@cli:'try'(
                                                                check_tailwind_integrity(
                                                                    Bin,
                                                                    Hash
                                                                ),
                                                                fun(_) ->
                                                                    lustre_dev_tools@cli:'try'(
                                                                        write_tailwind(
                                                                            Bin,
                                                                            Outdir,
                                                                            Outfile
                                                                        ),
                                                                        fun(_) ->
                                                                            lustre_dev_tools@cli:'try'(
                                                                                set_file_permissions(
                                                                                    Outfile
                                                                                ),
                                                                                fun(
                                                                                    _
                                                                                ) ->
                                                                                    lustre_dev_tools@cli:success(
                                                                                        <<"Tailwind installed!"/utf8>>,
                                                                                        fun(
                                                                                            
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
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
            end
        end
    ).

-spec setup(binary(), binary()) -> lustre_dev_tools@cli:cli(nil).
setup(Os, Cpu) ->
    lustre_dev_tools@cli:do(
        download(Os, Cpu),
        fun(_) ->
            lustre_dev_tools@cli:do(
                write_tailwind_config(),
                fun(_) ->
                    lustre_dev_tools@cli:do(
                        display_next_steps(),
                        fun(_) -> lustre_dev_tools@cli:return(nil) end
                    )
                end
            )
        end
    ).
