-module(lustre_dev_tools@esbuild).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([download/2, bundle/3]).

-spec check_esbuild_exists(binary()) -> boolean().
check_esbuild_exists(Path) ->
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
    Base = <<"https://registry.npmjs.org/@esbuild/"/utf8>>,
    case {Os, Cpu} of
        {<<"android"/utf8>>, <<"arm"/utf8>>} ->
            {ok,
                {<<Base/binary, "android-arm/-/android-arm-0.19.10.tgz"/utf8>>,
                    <<"545CF157B0E42E407AC1412F73876119414314D9E31982EBD1E9073336DA5365"/utf8>>}};

        {<<"android"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary,
                        "android-arm64/-/android-arm64-0.19.10.tgz"/utf8>>,
                    <<"DFB0A873B1BB9698EF42561B9513FC4A7D8392CE84FBD44FC276883B82AB087E"/utf8>>}};

        {<<"android"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "android-x64/-/android-x64-0.19.10.tgz"/utf8>>,
                    <<"B5D9D170F469BE483F3E16DA6033DFD064ED3DF788C6DC238BA6FE3232BF5653"/utf8>>}};

        {<<"darwin"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-arm64/-/darwin-arm64-0.19.10.tgz"/utf8>>,
                    <<"8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9"/utf8>>}};

        {<<"darwin"/utf8>>, <<"amd64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-arm64/-/darwin-arm64-0.19.10.tgz"/utf8>>,
                    <<"8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9"/utf8>>}};

        {<<"darwin"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-arm64/-/darwin-arm64-0.19.10.tgz"/utf8>>,
                    <<"8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9"/utf8>>}};

        {<<"darwin"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-x64/-/darwin-x64-0.19.10.tgz"/utf8>>,
                    <<"4AEC252E72CD56FD31341D960D8DCF0BEBFB858587061FE36B9047FD591C68A3"/utf8>>}};

        {<<"freebsd"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary,
                        "freebsd-arm64/-/freebsd-arm64-0.19.10.tgz"/utf8>>,
                    <<"739E9F6DD3121DB0CD03B70B14C4D17A1854970272B0F988BD035AA876BE254B"/utf8>>}};

        {<<"freebsd"/utf8>>, <<"amd64"/utf8>>} ->
            {ok,
                {<<Base/binary, "freebsd-x64/-/freebsd-x64-0.19.10.tgz"/utf8>>,
                    <<"3E937C849B21B89244A8A62B473E36EEFE793F5BDF602BEDBB314DD33DDBE7EE"/utf8>>}};

        {<<"linux"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm64/-/linux-arm64-0.19.10.tgz"/utf8>>,
                    <<"D3523B8F7B2540BA5A15C4EE4C747B31DFDC496C7A8A3F3FB0ECCB3008647DB7"/utf8>>}};

        {<<"linux"/utf8>>, <<"arm"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm/-/linux-arm-0.19.10.tgz"/utf8>>,
                    <<"99EEB37F5C1AB8750D9CAB6AB04469EF5CA32847B25E1461215276920AFB01B2"/utf8>>}};

        {<<"linux"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm64/-/linux-arm64-0.19.10.tgz"/utf8>>,
                    <<"D3523B8F7B2540BA5A15C4EE4C747B31DFDC496C7A8A3F3FB0ECCB3008647DB7"/utf8>>}};

        {<<"linux"/utf8>>, <<"ia32"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-ia32/-/linux-ia32-0.19.10.tgz"/utf8>>,
                    <<"3D69F7B90C62E6D94140355A92EDB15B8BFB934096C6E518BE41DAD6249BF38E"/utf8>>}};

        {<<"linux"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-x64/-/linux-x64-0.19.10.tgz"/utf8>>,
                    <<"73CA82A3C9049315027E60A50AF53C2ABFDE678BF66562B407FACA7FD3FAD6F4"/utf8>>}};

        {<<"linux"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-x64/-/linux-x64-0.19.10.tgz"/utf8>>,
                    <<"73CA82A3C9049315027E60A50AF53C2ABFDE678BF66562B407FACA7FD3FAD6F4"/utf8>>}};

        {<<"win32"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-arm64/-/win32-arm64-0.19.10.tgz"/utf8>>,
                    <<"2D0EC6ED7C5BA6F2D99CBB1428C1FAABFA7D42E7435BC40474C5787DCD1FF37C"/utf8>>}};

        {<<"win32"/utf8>>, <<"ia32"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-ia32/-/win32-ia32-0.19.10.tgz"/utf8>>,
                    <<"5BFBF08A8EDC16D53FE2103C68705DC3B4ABDFA6C44919B9602495ABA523BA46"/utf8>>}};

        {<<"win32"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-x64/-/win32-x64-0.19.10.tgz"/utf8>>,
                    <<"03EFF9A74ED7C72C8E4ACE85F6BFD2D097169D8D6E7D691AE1D7959F2912B785"/utf8>>}};

        {<<"win32"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-x64/-/win32-x64-0.19.10.tgz"/utf8>>,
                    <<"03EFF9A74ED7C72C8E4ACE85F6BFD2D097169D8D6E7D691AE1D7959F2912B785"/utf8>>}};

        {<<"netbsd"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "netbsd-x64/-/netbsd-x64-0.19.10.tgz"/utf8>>,
                    <<"C8F6E2CB79B1DDC2AD42C0AE25FB2A769A989E36B917B231CF9847B683D6DD8D"/utf8>>}};

        {<<"openbsd"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "openbsd-x64/-/openbsd-x64-0.19.10.tgz"/utf8>>,
                    <<"AFEBEAD35BB5A1B921C126E70E0D76CF04DB64FA53C60E0779816CFA9E1F9A11"/utf8>>}};

        {<<"sunos"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "sunos-x64/-/sunos-x64-0.19.10.tgz"/utf8>>,
                    <<"B1E9F969433574BD43A293FA3A3C71C88B8C4CF841957DAAA2CF83A90ADAAB7E"/utf8>>}};

        {_, _} ->
            {error, {unknown_platform, <<"esbuild"/utf8>>, Os, Cpu}}
    end.

-spec check_esbuild_integrity(bitstring(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
check_esbuild_integrity(Bin, Expected_hash) ->
    Hash = gleam_crypto_ffi:hash(sha256, Bin),
    Hash_string = binary:encode_hex(Hash),
    case Hash_string =:= Expected_hash of
        true ->
            {ok, nil};

        false ->
            {error, invalid_esbuild_binary}
    end.

-spec write_esbuild(bitstring(), binary(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
write_esbuild(Bin, Outdir, Outfile) ->
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

-spec exec_esbuild(binary(), list(binary())) -> {ok, binary()} |
    {error, lustre_dev_tools@error:error()}.
exec_esbuild(Root, Options) ->
    _pipe = lustre_dev_tools_ffi:exec(
        <<"./build/.lustre/bin/esbuild"/utf8>>,
        Options,
        Root
    ),
    gleam@result:map_error(
        _pipe,
        fun(Pair) -> {bundle_error, erlang:element(2, Pair)} end
    ).

-spec get_esbuild(binary()) -> {ok, bitstring()} |
    {error, lustre_dev_tools@error:error()}.
get_esbuild(Url) ->
    _pipe = lustre_dev_tools_ffi:get_esbuild(Url),
    gleam@result:map_error(_pipe, fun(Field@0) -> {network_error, Field@0} end).

-spec unzip_esbuild(bitstring()) -> {ok, bitstring()} |
    {error, lustre_dev_tools@error:error()}.
unzip_esbuild(Gzip) ->
    _pipe = lustre_dev_tools_ffi:unzip_esbuild(Gzip),
    gleam@result:map_error(_pipe, fun(Field@0) -> {unzip_error, Field@0} end).

-spec download(binary(), binary()) -> lustre_dev_tools@cli:cli(nil).
download(Os, Cpu) ->
    lustre_dev_tools@cli:log(
        <<"Downloading esbuild"/utf8>>,
        fun() ->
            Outdir = filepath:join(
                lustre_dev_tools@project:root(),
                <<"build/.lustre/bin"/utf8>>
            ),
            Outfile = filepath:join(Outdir, <<"esbuild"/utf8>>),
            case check_esbuild_exists(Outfile) of
                true ->
                    lustre_dev_tools@cli:success(
                        <<"Esbuild already installed!"/utf8>>,
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
                                    Max_url_size = (lustre_dev_tools@utils:term_width(
                                        
                                    )
                                    - 17)
                                    - 2,
                                    Shortened_url = lustre_dev_tools@utils:shorten_url(
                                        Url,
                                        Max_url_size
                                    ),
                                    lustre_dev_tools@cli:log(
                                        <<"Downloading from "/utf8,
                                            Shortened_url/binary>>,
                                        fun() ->
                                            lustre_dev_tools@cli:'try'(
                                                get_esbuild(Url),
                                                fun(Tarball) ->
                                                    lustre_dev_tools@cli:log(
                                                        <<"Checking the downloaded tarball"/utf8>>,
                                                        fun() ->
                                                            lustre_dev_tools@cli:'try'(
                                                                check_esbuild_integrity(
                                                                    Tarball,
                                                                    Hash
                                                                ),
                                                                fun(_) ->
                                                                    lustre_dev_tools@cli:log(
                                                                        <<"Unzipping esbuild"/utf8>>,
                                                                        fun() ->
                                                                            lustre_dev_tools@cli:'try'(
                                                                                unzip_esbuild(
                                                                                    Tarball
                                                                                ),
                                                                                fun(
                                                                                    Bin
                                                                                ) ->
                                                                                    lustre_dev_tools@cli:'try'(
                                                                                        write_esbuild(
                                                                                            Bin,
                                                                                            Outdir,
                                                                                            Outfile
                                                                                        ),
                                                                                        fun(
                                                                                            _
                                                                                        ) ->
                                                                                            lustre_dev_tools@cli:'try'(
                                                                                                set_file_permissions(
                                                                                                    Outfile
                                                                                                ),
                                                                                                fun(
                                                                                                    _
                                                                                                ) ->
                                                                                                    lustre_dev_tools@cli:success(
                                                                                                        <<"Esbuild installed!"/utf8>>,
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
                            )
                        end
                    )
            end
        end
    ).

-spec bundle(binary(), binary(), boolean()) -> lustre_dev_tools@cli:cli(nil).
bundle(Input_file, Output_file, Minify) ->
    lustre_dev_tools@cli:do(
        download(lustre_dev_tools_ffi:get_os(), lustre_dev_tools_ffi:get_cpu()),
        fun(_) ->
            lustre_dev_tools@cli:'try'(
                lustre_dev_tools@project:build(),
                fun(_) ->
                    Root = lustre_dev_tools@project:root(),
                    Flags = [<<"--bundle"/utf8>>,
                        <<"--external:node:*"/utf8>>,
                        <<"--format=esm"/utf8>>,
                        <<"--outfile="/utf8, Output_file/binary>>],
                    Options = case Minify of
                        true ->
                            [Input_file, <<"--minify"/utf8>> | Flags];

                        false ->
                            [Input_file | Flags]
                    end,
                    lustre_dev_tools@cli:log(
                        <<"Bundling with esbuild"/utf8>>,
                        fun() ->
                            lustre_dev_tools@cli:do(
                                lustre_dev_tools@esbuild@preprocess:copy_deep_ffi(
                                    
                                ),
                                fun(_) ->
                                    lustre_dev_tools@cli:'try'(
                                        exec_esbuild(Root, Options),
                                        fun(_) ->
                                            lustre_dev_tools@cli:success(
                                                <<<<"Bundle produced at `"/utf8,
                                                        Output_file/binary>>/binary,
                                                    "`"/utf8>>,
                                                fun() ->
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
    ).
