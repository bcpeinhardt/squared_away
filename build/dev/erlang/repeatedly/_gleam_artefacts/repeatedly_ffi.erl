-module(repeatedly_ffi).

% Public API
-export([call/3, stop/1, replace/2, update_state/2]).

% Gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, { state, function, delay_ms, index }).

call(DelayMs, State, Function) ->
    S = #state{
        function = Function,
        delay_ms = DelayMs,
        state = State,
        index = 0
    },
    {ok, Pid} = gen_server:start_link(?MODULE, S, []),
    Pid.

replace(Pid, Function) ->
    erlang:send(Pid, {replace, Function}).

update_state(Pid, F) ->
    erlang:send(Pid, {update_state, F}).

stop(Pid) when is_pid(Pid) ->
    try
        gen_server:stop(Pid),
        nil
    catch
        exit:noproc -> nil
    end.

init(State) ->
    schedule_next(State),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = #state{function = F, index = I, state = S}) ->
    New = F(S, I),
    schedule_next(State),
    {noreply, State#state{state = New, index = I + 1}};
handle_info({replace, F}, State) ->
    {noreply, State#state{function = F}};
handle_info({update_state, F}, State) ->
    {noreply, State#state{state = F(State#state.state)}};
handle_info(_Info, State) ->
    {noreply, State}.

schedule_next(#state{delay_ms = DelayMs}) ->
    erlang:send_after(DelayMs, self(), tick).
