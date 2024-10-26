-module(gleam@otp@supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([add/2, supervisor/1, worker/1, returning/2, start_spec/1, start/1, application_stopped/0, to_erlang_start_result/1]).
-export_type([spec/2, children/1, child_spec/3, child_start_error/0, message/0, instruction/0, state/1, starter/1, child/1, handle_exit_error/0, application_start_mode/0, application_stop/0]).

-type spec(GXW, GXX) :: {spec,
        GXW,
        integer(),
        integer(),
        fun((children(GXW)) -> children(GXX))}.

-opaque children(GXY) :: {ready, starter(GXY)} | {failed, child_start_error()}.

-opaque child_spec(GXZ, GYA, GYB) :: {child_spec,
        fun((GYA) -> {ok, gleam@erlang@process:subject(GXZ)} |
            {error, gleam@otp@actor:start_error()}),
        fun((GYA, gleam@erlang@process:subject(GXZ)) -> GYB)}.

-type child_start_error() :: {child_start_error,
        gleam@option:option(gleam@erlang@process:pid_()),
        gleam@otp@actor:start_error()}.

-opaque message() :: {exit, gleam@erlang@process:exit_message()} |
    {retry_restart, gleam@erlang@process:pid_()}.

-type instruction() :: start_all | {start_from, gleam@erlang@process:pid_()}.

-type state(GYC) :: {state,
        gleam@otp@intensity_tracker:intensity_tracker(),
        starter(GYC),
        gleam@erlang@process:subject(gleam@erlang@process:pid_())}.

-type starter(GYD) :: {starter,
        GYD,
        gleam@option:option(fun((instruction()) -> {ok,
                {starter(GYD), instruction()}} |
            {error, child_start_error()}))}.

-type child(GYE) :: {child, gleam@erlang@process:pid_(), GYE}.

-type handle_exit_error() :: {restart_failed,
        gleam@erlang@process:pid_(),
        gleam@otp@intensity_tracker:intensity_tracker()} |
    too_many_restarts.

-type application_start_mode() :: normal |
    {takeover, gleam@erlang@node:node_()} |
    {failover, gleam@erlang@node:node_()}.

-type application_stop() :: any().

-spec start_child(child_spec(any(), GYI, GYJ), GYI) -> {ok, child(GYJ)} |
    {error, child_start_error()}.
start_child(Child_spec, Argument) ->
    gleam@result:then(
        begin
            _pipe = (erlang:element(2, Child_spec))(Argument),
            gleam@result:map_error(
                _pipe,
                fun(_capture) -> {child_start_error, none, _capture} end
            )
        end,
        fun(Subject) ->
            {ok,
                {child,
                    gleam@erlang@process:subject_owner(Subject),
                    (erlang:element(3, Child_spec))(Argument, Subject)}}
        end
    ).

-spec shutdown_child(
    gleam@erlang@process:pid_(),
    child_spec(any(), any(), any())
) -> nil.
shutdown_child(Pid, _) ->
    gleam@erlang@process:send_exit(Pid).

-spec perform_instruction_for_child(
    GYW,
    instruction(),
    child_spec(any(), GYW, GYY),
    child(GYY)
) -> {ok, {child(GYY), instruction()}} | {error, child_start_error()}.
perform_instruction_for_child(Argument, Instruction, Child_spec, Child) ->
    Current = erlang:element(2, Child),
    case Instruction of
        {start_from, Target} when Target =/= Current ->
            {ok, {Child, Instruction}};

        _ ->
            shutdown_child(Current, Child_spec),
            gleam@result:then(
                start_child(Child_spec, Argument),
                fun(Child@1) -> {ok, {Child@1, start_all}} end
            )
    end.

-spec add_child_to_starter(
    starter(GZG),
    child_spec(any(), GZG, GZJ),
    child(GZJ)
) -> starter(GZJ).
add_child_to_starter(Starter, Child_spec, Child) ->
    Starter@3 = fun(Instruction) ->
        gleam@result:then(case erlang:element(3, Starter) of
                {some, Start} ->
                    Start(Instruction);

                none ->
                    {ok, {Starter, Instruction}}
            end, fun(_use0) ->
                {Starter@1, Instruction@1} = _use0,
                gleam@result:then(
                    perform_instruction_for_child(
                        erlang:element(2, Starter@1),
                        Instruction@1,
                        Child_spec,
                        Child
                    ),
                    fun(_use0@1) ->
                        {Child@1, Instruction@2} = _use0@1,
                        Starter@2 = add_child_to_starter(
                            Starter@1,
                            Child_spec,
                            Child@1
                        ),
                        {ok, {Starter@2, Instruction@2}}
                    end
                )
            end)
    end,
    {starter, erlang:element(3, Child), {some, Starter@3}}.

-spec start_and_add_child(starter(GZP), child_spec(any(), GZP, GZS)) -> children(GZS).
start_and_add_child(State, Child_spec) ->
    case start_child(Child_spec, erlang:element(2, State)) of
        {ok, Child} ->
            {ready, add_child_to_starter(State, Child_spec, Child)};

        {error, Reason} ->
            {failed, Reason}
    end.

-spec add(children(GZX), child_spec(any(), GZX, HAA)) -> children(HAA).
add(Children, Child_spec) ->
    case Children of
        {failed, Fail} ->
            {failed, Fail};

        {ready, State} ->
            start_and_add_child(State, Child_spec)
    end.

-spec supervisor(
    fun((HAF) -> {ok, gleam@erlang@process:subject(HAG)} |
        {error, gleam@otp@actor:start_error()})
) -> child_spec(HAG, HAF, HAF).
supervisor(Start) ->
    {child_spec, Start, fun(Argument, _) -> Argument end}.

-spec worker(
    fun((HAN) -> {ok, gleam@erlang@process:subject(HAO)} |
        {error, gleam@otp@actor:start_error()})
) -> child_spec(HAO, HAN, HAN).
worker(Start) ->
    {child_spec, Start, fun(Argument, _) -> Argument end}.

-spec returning(
    child_spec(HAV, HAW, any()),
    fun((HAW, gleam@erlang@process:subject(HAV)) -> HBC)
) -> child_spec(HAV, HAW, HBC).
returning(Child, Updater) ->
    {child_spec, erlang:element(2, Child), Updater}.

-spec init(spec(any(), HBH)) -> gleam@otp@actor:init_result(state(HBH), message()).
init(Spec) ->
    Retry = gleam@erlang@process:new_subject(),
    gleam_erlang_ffi:trap_exits(true),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = gleam@erlang@process:selecting(
            _pipe,
            Retry,
            fun(Field@0) -> {retry_restart, Field@0} end
        ),
        gleam@erlang@process:selecting_trapped_exits(
            _pipe@1,
            fun(Field@0) -> {exit, Field@0} end
        )
    end,
    Result = begin
        _pipe@2 = {starter, erlang:element(2, Spec), none},
        _pipe@3 = {ready, _pipe@2},
        (erlang:element(5, Spec))(_pipe@3)
    end,
    case Result of
        {ready, Starter} ->
            Restarts = gleam@otp@intensity_tracker:new(
                erlang:element(3, Spec),
                erlang:element(4, Spec)
            ),
            State = {state, Restarts, Starter, Retry},
            {ready, State, Selector};

        {failed, Error} ->
            {failed, case erlang:element(3, Error) of
                    init_timeout ->
                        <<"Child initialisation timed out"/utf8>>;

                    {init_crashed, Reason} ->
                        gleam@string:append(
                            <<"Child crashed during initialisation: "/utf8>>,
                            gleam@string:inspect(Reason)
                        );

                    {init_failed, Reason@1} ->
                        gleam@string:append(
                            <<"Child failed to start during initialisation: "/utf8>>,
                            gleam@string:inspect(Reason@1)
                        )
                end}
    end.

-spec handle_exit(gleam@erlang@process:pid_(), state(HBN)) -> gleam@otp@actor:next(message(), state(HBN)).
handle_exit(Pid, State) ->
    Outcome = begin
        _assert_subject = erlang:element(3, erlang:element(3, State)),
        {some, Start} = case _assert_subject of
            {some, _} -> _assert_subject;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Assertion pattern match failed"/utf8>>,
                            value => _assert_fail,
                            module => <<"gleam/otp/supervisor"/utf8>>,
                            function => <<"handle_exit"/utf8>>,
                            line => 308})
        end,
        gleam@result:then(
            begin
                _pipe = erlang:element(2, State),
                _pipe@1 = gleam@otp@intensity_tracker:add_event(_pipe),
                gleam@result:map_error(_pipe@1, fun(_) -> too_many_restarts end)
            end,
            fun(Restarts) ->
                gleam@result:then(
                    begin
                        _pipe@2 = Start({start_from, Pid}),
                        gleam@result:map_error(
                            _pipe@2,
                            fun(E) ->
                                {restart_failed,
                                    gleam@option:unwrap(
                                        erlang:element(2, E),
                                        Pid
                                    ),
                                    Restarts}
                            end
                        )
                    end,
                    fun(_use0) ->
                        {Starter, _} = _use0,
                        {ok,
                            erlang:setelement(
                                2,
                                erlang:setelement(3, State, Starter),
                                Restarts
                            )}
                    end
                )
            end
        )
    end,
    case Outcome of
        {ok, State@1} ->
            gleam@otp@actor:continue(State@1);

        {error, {restart_failed, Failed_child, Restarts@1}} ->
            gleam@erlang@process:send(erlang:element(4, State), Failed_child),
            State@2 = erlang:setelement(2, State, Restarts@1),
            gleam@otp@actor:continue(State@2);

        {error, too_many_restarts} ->
            {stop,
                {abnormal,
                    <<"Child processes restarted too many times within allowed period"/utf8>>}}
    end.

-spec loop(message(), state(HBS)) -> gleam@otp@actor:next(message(), state(HBS)).
loop(Message, State) ->
    case Message of
        {exit, Exit_message} ->
            handle_exit(erlang:element(2, Exit_message), State);

        {retry_restart, Pid} ->
            handle_exit(Pid, State)
    end.

-spec start_spec(spec(any(), any())) -> {ok,
        gleam@erlang@process:subject(message())} |
    {error, gleam@otp@actor:start_error()}.
start_spec(Spec) ->
    gleam@otp@actor:start_spec(
        {spec, fun() -> init(Spec) end, 60000, fun loop/2}
    ).

-spec start(fun((children(nil)) -> children(any()))) -> {ok,
        gleam@erlang@process:subject(message())} |
    {error, gleam@otp@actor:start_error()}.
start(Init) ->
    start_spec({spec, nil, 1, 5, Init}).

-spec application_stopped() -> application_stop().
application_stopped() ->
    gleam_otp_external:application_stopped().

-spec to_erlang_start_result(
    {ok, gleam@erlang@process:subject(any())} |
        {error, gleam@otp@actor:start_error()}
) -> {ok, gleam@erlang@process:pid_()} | {error, gleam@dynamic:dynamic_()}.
to_erlang_start_result(Res) ->
    gleam@otp@actor:to_erlang_start_result(Res).
