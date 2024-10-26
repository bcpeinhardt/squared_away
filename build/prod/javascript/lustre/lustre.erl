-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([application/3, element/1, simple/3, component/4, start_actor/2, start_server_component/2, register/2, dispatch/1, shutdown/0, is_browser/0, start/3, is_registered/1]).
-export_type([app/3, client_spa/0, server_component/0, error/0]).

-opaque app(POL, POM, PON) :: {app,
        fun((POL) -> {POM, lustre@effect:effect(PON)}),
        fun((POM, PON) -> {POM, lustre@effect:effect(PON)}),
        fun((POM) -> lustre@internals@vdom:element(PON)),
        gleam@option:option(gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
                PON} |
            {error, list(gleam@dynamic:decode_error())})))}.

-type client_spa() :: any().

-type server_component() :: any().

-type error() :: {actor_error, gleam@otp@actor:start_error()} |
    {bad_component_name, binary()} |
    {component_already_registered, binary()} |
    {element_not_found, binary()} |
    not_a_browser |
    not_erlang.

-spec application(
    fun((PPG) -> {PPH, lustre@effect:effect(PPI)}),
    fun((PPH, PPI) -> {PPH, lustre@effect:effect(PPI)}),
    fun((PPH) -> lustre@internals@vdom:element(PPI))
) -> app(PPG, PPH, PPI).
application(Init, Update, View) ->
    {app, Init, Update, View, none}.

-spec element(lustre@internals@vdom:element(POU)) -> app(nil, nil, POU).
element(Html) ->
    Init = fun(_) -> {nil, lustre@effect:none()} end,
    Update = fun(_, _) -> {nil, lustre@effect:none()} end,
    View = fun(_) -> Html end,
    application(Init, Update, View).

-spec simple(
    fun((POZ) -> PPA),
    fun((PPA, PPB) -> PPA),
    fun((PPA) -> lustre@internals@vdom:element(PPB))
) -> app(POZ, PPA, PPB).
simple(Init, Update, View) ->
    Init@1 = fun(Flags) -> {Init(Flags), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-spec component(
    fun((PPP) -> {PPQ, lustre@effect:effect(PPR)}),
    fun((PPQ, PPR) -> {PPQ, lustre@effect:effect(PPR)}),
    fun((PPQ) -> lustre@internals@vdom:element(PPR)),
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, PPR} |
        {error, list(gleam@dynamic:decode_error())}))
) -> app(PPP, PPQ, PPR).
component(Init, Update, View, On_attribute_change) ->
    {app, Init, Update, View, {some, On_attribute_change}}.

-spec do_start(app(PQL, any(), PQN), binary(), PQL) -> {ok,
        fun((lustre@internals@runtime:action(PQN, client_spa())) -> nil)} |
    {error, error()}.
do_start(_, _, _) ->
    {error, not_a_browser}.

-spec do_start_actor(app(PRQ, any(), PRS), PRQ) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(PRS, server_component()))} |
    {error, error()}.
do_start_actor(App, Flags) ->
    On_attribute_change = gleam@option:unwrap(
        erlang:element(5, App),
        gleam@dict:new()
    ),
    _pipe = (erlang:element(2, App))(Flags),
    _pipe@1 = lustre@internals@runtime:start(
        _pipe,
        erlang:element(3, App),
        erlang:element(4, App),
        On_attribute_change
    ),
    gleam@result:map_error(_pipe@1, fun(Field@0) -> {actor_error, Field@0} end).

-spec start_actor(app(PRF, any(), PRH), PRF) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(PRH, server_component()))} |
    {error, error()}.
start_actor(App, Flags) ->
    do_start_actor(App, Flags).

-spec start_server_component(app(PQV, any(), PQX), PQV) -> {ok,
        fun((lustre@internals@runtime:action(PQX, server_component())) -> nil)} |
    {error, error()}.
start_server_component(App, Flags) ->
    gleam@result:map(
        start_actor(App, Flags),
        fun(Runtime) ->
            fun(_capture) -> gleam@otp@actor:send(Runtime, _capture) end
        end
    ).

-spec register(app(nil, any(), any()), binary()) -> {ok, nil} | {error, error()}.
register(_, _) ->
    {error, not_a_browser}.

-spec dispatch(PSI) -> lustre@internals@runtime:action(PSI, any()).
dispatch(Msg) ->
    {dispatch, Msg}.

-spec shutdown() -> lustre@internals@runtime:action(any(), any()).
shutdown() ->
    shutdown.

-spec is_browser() -> boolean().
is_browser() ->
    false.

-spec start(app(PQB, any(), PQD), binary(), PQB) -> {ok,
        fun((lustre@internals@runtime:action(PQD, client_spa())) -> nil)} |
    {error, error()}.
start(App, Selector, Flags) ->
    gleam@bool:guard(
        not is_browser(),
        {error, not_a_browser},
        fun() -> do_start(App, Selector, Flags) end
    ).

-spec is_registered(binary()) -> boolean().
is_registered(_) ->
    false.
