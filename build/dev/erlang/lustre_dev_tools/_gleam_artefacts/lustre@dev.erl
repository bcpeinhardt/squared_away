-module(lustre@dev).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec main() -> nil.
main() ->
    Args = erlang:element(4, argv:load()),
    _pipe = glint:new(),
    _pipe@1 = glint:as_module(_pipe),
    _pipe@2 = glint:with_name(_pipe@1, <<"lustre/dev"/utf8>>),
    _pipe@3 = glint:path_help(
        _pipe@2,
        [<<"add"/utf8>>],
        <<"
Commands for adding external binaries to your project. These are run and managed
by Lustre, and while not typically intended to be run manually, they can be found
inside `build/.lustre/bin`.
  "/utf8>>
    ),
    _pipe@4 = glint:add(
        _pipe@3,
        [<<"add"/utf8>>, <<"esbuild"/utf8>>],
        lustre_dev_tools@cli@add:esbuild()
    ),
    _pipe@5 = glint:add(
        _pipe@4,
        [<<"add"/utf8>>, <<"tailwind"/utf8>>],
        lustre_dev_tools@cli@add:tailwind()
    ),
    _pipe@6 = glint:add(
        _pipe@5,
        [<<"build"/utf8>>],
        lustre_dev_tools@cli@build:app()
    ),
    _pipe@7 = glint:add(
        _pipe@6,
        [<<"build"/utf8>>, <<"app"/utf8>>],
        lustre_dev_tools@cli@build:app()
    ),
    _pipe@8 = glint:add(
        _pipe@7,
        [<<"build"/utf8>>, <<"component"/utf8>>],
        lustre_dev_tools@cli@build:component()
    ),
    _pipe@9 = glint:add(
        _pipe@8,
        [<<"start"/utf8>>],
        lustre_dev_tools@cli@start:run()
    ),
    glint:run(_pipe@9, Args).
