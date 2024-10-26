-module(lustre_dev_tools@cli@flag).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([esbuild_os/0, esbuild_cpu/0, tailwind_os/0, tailwind_cpu/0, minify/0, tailwind_entry/0, outdir/0, ext/0, detect_tailwind/0, port/0, proxy_from/0, proxy_to/0]).

-spec esbuild_os() -> glint:flag(binary()).
esbuild_os() ->
    Description = <<"Override the automatic OS detection."/utf8>>,
    Allowed = [<<"android"/utf8>>,
        <<"darwin"/utf8>>,
        <<"freebsd"/utf8>>,
        <<"linux"/utf8>>,
        <<"win32"/utf8>>,
        <<"netbsd"/utf8>>,
        <<"openbsd"/utf8>>,
        <<"sunos"/utf8>>],
    _pipe = glint:string_flag(<<"os"/utf8>>),
    _pipe@1 = glint:flag_help(_pipe, Description),
    glint:flag_constraint(_pipe@1, glint@constraint:one_of(Allowed)).

-spec esbuild_cpu() -> glint:flag(binary()).
esbuild_cpu() ->
    Description = <<"Override the automatic CPU architecture detection."/utf8>>,
    Allowed = [<<"aarch64"/utf8>>,
        <<"amd64"/utf8>>,
        <<"arm"/utf8>>,
        <<"arm64"/utf8>>,
        <<"ia32"/utf8>>,
        <<"x64"/utf8>>,
        <<"x86_64"/utf8>>],
    _pipe = glint:string_flag(<<"cpu"/utf8>>),
    _pipe@1 = glint:flag_help(_pipe, Description),
    glint:flag_constraint(_pipe@1, glint@constraint:one_of(Allowed)).

-spec tailwind_os() -> glint:flag(binary()).
tailwind_os() ->
    Description = <<"Override the automatic OS detection."/utf8>>,
    Allowed = [<<"linux"/utf8>>, <<"win32"/utf8>>, <<"darwin"/utf8>>],
    _pipe = glint:string_flag(<<"os"/utf8>>),
    _pipe@1 = glint:flag_help(_pipe, Description),
    glint:flag_constraint(_pipe@1, glint@constraint:one_of(Allowed)).

-spec tailwind_cpu() -> glint:flag(binary()).
tailwind_cpu() ->
    Description = <<"Override the automatic CPU architecture detection."/utf8>>,
    Allowed = [<<"armv7"/utf8>>,
        <<"arm64"/utf8>>,
        <<"x64"/utf8>>,
        <<"x86_64"/utf8>>,
        <<"aarch64"/utf8>>],
    _pipe = glint:string_flag(<<"cpu"/utf8>>),
    _pipe@1 = glint:flag_help(_pipe, Description),
    glint:flag_constraint(_pipe@1, glint@constraint:one_of(Allowed)).

-spec minify() -> glint:flag(boolean()).
minify() ->
    Description = <<"Minify the output, renaming variables and removing whitespace."/utf8>>,
    _pipe = glint:bool_flag(<<"minify"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec tailwind_entry() -> glint:flag(binary()).
tailwind_entry() ->
    Description = <<"Use a custom CSS file as the entry to a Tailwind CSS bundle."/utf8>>,
    _pipe = glint:string_flag(<<"tailwind-entry"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec outdir() -> glint:flag(binary()).
outdir() ->
    Description = <<"Use a custom directory as the destination for any built files."/utf8>>,
    _pipe = glint:string_flag(<<"outdir"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec ext() -> glint:flag(binary()).
ext() ->
    Description = <<"Use a file extension other than 'mjs' for the built JavaScript."/utf8>>,
    _pipe = glint:string_flag(<<"ext"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec detect_tailwind() -> glint:flag(boolean()).
detect_tailwind() ->
    Description = <<"Detect and build Tailwind styles automatically."/utf8>>,
    _pipe = glint:bool_flag(<<"detect-tailwind"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec port() -> glint:flag(integer()).
port() ->
    Description = <<"Specify server port. If the port is taken the dev server will not start."/utf8>>,
    _pipe = glint:int_flag(<<"port"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec proxy_from() -> glint:flag(binary()).
proxy_from() ->
    Description = <<"Proxy requests that start with this path to the URL specified by the --proxy-to flag."/utf8>>,
    _pipe = glint:string_flag(<<"proxy-from"/utf8>>),
    glint:flag_help(_pipe, Description).

-spec proxy_to() -> glint:flag(binary()).
proxy_to() ->
    Description = <<"Proxy requests that start with the path specified by the --proxy-from flag to this URL."/utf8>>,
    _pipe = glint:string_flag(<<"proxy-to"/utf8>>),
    glint:flag_help(_pipe, Description).
