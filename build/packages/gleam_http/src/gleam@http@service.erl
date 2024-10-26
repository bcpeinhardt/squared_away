-module(gleam@http@service).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([map_response_body/2, prepend_response_header/3, method_override/1]).

-spec map_response_body(
    fun((GRX) -> gleam@http@response:response(GRJ)),
    fun((GRJ) -> GRK)
) -> fun((GRX) -> gleam@http@response:response(GRK)).
map_response_body(Service, Mapper) ->
    fun(Req) -> _pipe = Req,
        _pipe@1 = Service(_pipe),
        gleam@http@response:map(_pipe@1, Mapper) end.

-spec prepend_response_header(
    fun((GSB) -> gleam@http@response:response(GSD)),
    binary(),
    binary()
) -> fun((GSB) -> gleam@http@response:response(GSD)).
prepend_response_header(Service, Key, Value) ->
    fun(Req) -> _pipe = Req,
        _pipe@1 = Service(_pipe),
        gleam@http@response:prepend_header(_pipe@1, Key, Value) end.

-spec ensure_post(gleam@http@request:request(GRO)) -> {ok,
        gleam@http@request:request(GRO)} |
    {error, nil}.
ensure_post(Req) ->
    case erlang:element(2, Req) of
        post ->
            {ok, Req};

        _ ->
            {error, nil}
    end.

-spec get_override_method(gleam@http@request:request(any())) -> {ok,
        gleam@http:method()} |
    {error, nil}.
get_override_method(Request) ->
    gleam@result:then(
        gleam@http@request:get_query(Request),
        fun(Query_params) ->
            gleam@result:then(
                gleam@list:key_find(Query_params, <<"_method"/utf8>>),
                fun(Method) ->
                    gleam@result:then(
                        gleam@http:parse_method(Method),
                        fun(Method@1) -> case Method@1 of
                                put ->
                                    {ok, Method@1};

                                patch ->
                                    {ok, Method@1};

                                delete ->
                                    {ok, Method@1};

                                _ ->
                                    {error, nil}
                            end end
                    )
                end
            )
        end
    ).

-spec method_override(fun((gleam@http@request:request(GTK)) -> GTS)) -> fun((gleam@http@request:request(GTK)) -> GTS).
method_override(Service) ->
    fun(Request) -> _pipe = Request,
        _pipe@1 = ensure_post(_pipe),
        _pipe@2 = gleam@result:then(_pipe@1, fun get_override_method/1),
        _pipe@3 = gleam@result:map(
            _pipe@2,
            fun(_capture) ->
                gleam@http@request:set_method(Request, _capture)
            end
        ),
        _pipe@4 = gleam@result:unwrap(_pipe@3, Request),
        Service(_pipe@4) end.
