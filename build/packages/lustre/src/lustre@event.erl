-module(lustre@event).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([emit/2, on/2, on_click/1, on_mouse_down/1, on_mouse_up/1, on_mouse_enter/1, on_mouse_leave/1, on_mouse_over/1, on_mouse_out/1, on_keypress/1, on_keydown/1, on_keyup/1, on_focus/1, on_blur/1, value/1, on_input/1, checked/1, on_check/1, mouse_position/1, prevent_default/1, on_submit/1, stop_propagation/1]).

-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-spec on(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, RRH} |
        {error, list(gleam@dynamic:decode_error())})
) -> lustre@internals@vdom:attribute(RRH).
on(Name, Handler) ->
    lustre@attribute:on(Name, Handler).

-spec on_click(RRK) -> lustre@internals@vdom:attribute(RRK).
on_click(Msg) ->
    on(<<"click"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_mouse_down(RRM) -> lustre@internals@vdom:attribute(RRM).
on_mouse_down(Msg) ->
    on(<<"mousedown"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_mouse_up(RRO) -> lustre@internals@vdom:attribute(RRO).
on_mouse_up(Msg) ->
    on(<<"mouseup"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_mouse_enter(RRQ) -> lustre@internals@vdom:attribute(RRQ).
on_mouse_enter(Msg) ->
    on(<<"mouseenter"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_mouse_leave(RRS) -> lustre@internals@vdom:attribute(RRS).
on_mouse_leave(Msg) ->
    on(<<"mouseleave"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_mouse_over(RRU) -> lustre@internals@vdom:attribute(RRU).
on_mouse_over(Msg) ->
    on(<<"mouseover"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_mouse_out(RRW) -> lustre@internals@vdom:attribute(RRW).
on_mouse_out(Msg) ->
    on(<<"mouseout"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_keypress(fun((binary()) -> RRY)) -> lustre@internals@vdom:attribute(RRY).
on_keypress(Msg) ->
    on(<<"keypress"/utf8>>, fun(Event) -> _pipe = Event,
            _pipe@1 = (gleam@dynamic:field(
                <<"key"/utf8>>,
                fun gleam@dynamic:string/1
            ))(_pipe),
            gleam@result:map(_pipe@1, Msg) end).

-spec on_keydown(fun((binary()) -> RSA)) -> lustre@internals@vdom:attribute(RSA).
on_keydown(Msg) ->
    on(<<"keydown"/utf8>>, fun(Event) -> _pipe = Event,
            _pipe@1 = (gleam@dynamic:field(
                <<"key"/utf8>>,
                fun gleam@dynamic:string/1
            ))(_pipe),
            gleam@result:map(_pipe@1, Msg) end).

-spec on_keyup(fun((binary()) -> RSC)) -> lustre@internals@vdom:attribute(RSC).
on_keyup(Msg) ->
    on(<<"keyup"/utf8>>, fun(Event) -> _pipe = Event,
            _pipe@1 = (gleam@dynamic:field(
                <<"key"/utf8>>,
                fun gleam@dynamic:string/1
            ))(_pipe),
            gleam@result:map(_pipe@1, Msg) end).

-spec on_focus(RSK) -> lustre@internals@vdom:attribute(RSK).
on_focus(Msg) ->
    on(<<"focus"/utf8>>, fun(_) -> {ok, Msg} end).

-spec on_blur(RSM) -> lustre@internals@vdom:attribute(RSM).
on_blur(Msg) ->
    on(<<"blur"/utf8>>, fun(_) -> {ok, Msg} end).

-spec value(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, list(gleam@dynamic:decode_error())}.
value(Event) ->
    _pipe = Event,
    (gleam@dynamic:field(
        <<"target"/utf8>>,
        gleam@dynamic:field(<<"value"/utf8>>, fun gleam@dynamic:string/1)
    ))(_pipe).

-spec on_input(fun((binary()) -> RSE)) -> lustre@internals@vdom:attribute(RSE).
on_input(Msg) ->
    on(<<"input"/utf8>>, fun(Event) -> _pipe = value(Event),
            gleam@result:map(_pipe, Msg) end).

-spec checked(gleam@dynamic:dynamic_()) -> {ok, boolean()} |
    {error, list(gleam@dynamic:decode_error())}.
checked(Event) ->
    _pipe = Event,
    (gleam@dynamic:field(
        <<"target"/utf8>>,
        gleam@dynamic:field(<<"checked"/utf8>>, fun gleam@dynamic:bool/1)
    ))(_pipe).

-spec on_check(fun((boolean()) -> RSG)) -> lustre@internals@vdom:attribute(RSG).
on_check(Msg) ->
    on(<<"change"/utf8>>, fun(Event) -> _pipe = checked(Event),
            gleam@result:map(_pipe, Msg) end).

-spec mouse_position(gleam@dynamic:dynamic_()) -> {ok, {float(), float()}} |
    {error, list(gleam@dynamic:decode_error())}.
mouse_position(Event) ->
    gleam@result:then(
        (gleam@dynamic:field(<<"clientX"/utf8>>, fun gleam@dynamic:float/1))(
            Event
        ),
        fun(X) ->
            gleam@result:then(
                (gleam@dynamic:field(
                    <<"clientY"/utf8>>,
                    fun gleam@dynamic:float/1
                ))(Event),
                fun(Y) -> {ok, {X, Y}} end
            )
        end
    ).

-spec prevent_default(gleam@dynamic:dynamic_()) -> nil.
prevent_default(_) ->
    nil.

-spec on_submit(RSI) -> lustre@internals@vdom:attribute(RSI).
on_submit(Msg) ->
    on(
        <<"submit"/utf8>>,
        fun(Event) ->
            _ = prevent_default(Event),
            {ok, Msg}
        end
    ).

-spec stop_propagation(gleam@dynamic:dynamic_()) -> nil.
stop_propagation(_) ->
    nil.
