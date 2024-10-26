-module(gleam@httpc).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([dispatch_bits/2, configure/0, send_bits/1, verify_tls/2, dispatch/2, send/1]).
-export_type([charlist/0, erl_http_option/0, body_format/0, erl_option/0, erl_ssl_option/0, erl_verify_option/0, configuration/0]).

-type charlist() :: any().

-type erl_http_option() :: {ssl, list(erl_ssl_option())}.

-type body_format() :: binary.

-type erl_option() :: {body_format, body_format()}.

-type erl_ssl_option() :: {verify, erl_verify_option()}.

-type erl_verify_option() :: verify_none.

-opaque configuration() :: {builder, boolean()}.

-spec charlist_header({binary(), binary()}) -> {charlist(), charlist()}.
charlist_header(Header) ->
    {K, V} = Header,
    {erlang:binary_to_list(K), erlang:binary_to_list(V)}.

-spec string_header({charlist(), charlist()}) -> {binary(), binary()}.
string_header(Header) ->
    {K, V} = Header,
    {erlang:list_to_binary(K), erlang:list_to_binary(V)}.

-spec dispatch_bits(configuration(), gleam@http@request:request(bitstring())) -> {ok,
        gleam@http@response:response(bitstring())} |
    {error, gleam@dynamic:dynamic_()}.
dispatch_bits(Config, Req) ->
    Erl_url = begin
        _pipe = Req,
        _pipe@1 = gleam@http@request:to_uri(_pipe),
        _pipe@2 = gleam@uri:to_string(_pipe@1),
        erlang:binary_to_list(_pipe@2)
    end,
    Erl_headers = gleam@list:map(erlang:element(3, Req), fun charlist_header/1),
    Erl_http_options = case erlang:element(2, Config) of
        true ->
            [];

        false ->
            [{ssl, [{verify, verify_none}]}]
    end,
    Erl_options = [{body_format, binary}],
    gleam@result:then(case erlang:element(2, Req) of
            options ->
                Erl_req = {Erl_url, Erl_headers},
                httpc:request(
                    erlang:element(2, Req),
                    Erl_req,
                    Erl_http_options,
                    Erl_options
                );

            head ->
                Erl_req = {Erl_url, Erl_headers},
                httpc:request(
                    erlang:element(2, Req),
                    Erl_req,
                    Erl_http_options,
                    Erl_options
                );

            get ->
                Erl_req = {Erl_url, Erl_headers},
                httpc:request(
                    erlang:element(2, Req),
                    Erl_req,
                    Erl_http_options,
                    Erl_options
                );

            _ ->
                Erl_content_type = begin
                    _pipe@3 = Req,
                    _pipe@4 = gleam@http@request:get_header(
                        _pipe@3,
                        <<"content-type"/utf8>>
                    ),
                    _pipe@5 = gleam@result:unwrap(
                        _pipe@4,
                        <<"application/octet-stream"/utf8>>
                    ),
                    erlang:binary_to_list(_pipe@5)
                end,
                Erl_req@1 = {Erl_url,
                    Erl_headers,
                    Erl_content_type,
                    erlang:element(4, Req)},
                httpc:request(
                    erlang:element(2, Req),
                    Erl_req@1,
                    Erl_http_options,
                    Erl_options
                )
        end, fun(Response) ->
            {{_, Status, _}, Headers, Resp_body} = Response,
            {ok,
                {response,
                    Status,
                    gleam@list:map(Headers, fun string_header/1),
                    Resp_body}}
        end).

-spec configure() -> configuration().
configure() ->
    {builder, true}.

-spec send_bits(gleam@http@request:request(bitstring())) -> {ok,
        gleam@http@response:response(bitstring())} |
    {error, gleam@dynamic:dynamic_()}.
send_bits(Req) ->
    _pipe = configure(),
    dispatch_bits(_pipe, Req).

-spec verify_tls(configuration(), boolean()) -> configuration().
verify_tls(_, Which) ->
    {builder, Which}.

-spec dispatch(configuration(), gleam@http@request:request(binary())) -> {ok,
        gleam@http@response:response(binary())} |
    {error, gleam@dynamic:dynamic_()}.
dispatch(Config, Request) ->
    Request@1 = gleam@http@request:map(Request, fun gleam_stdlib:identity/1),
    gleam@result:'try'(
        dispatch_bits(Config, Request@1),
        fun(Resp) -> case gleam@bit_array:to_string(erlang:element(4, Resp)) of
                {ok, Body} ->
                    {ok, gleam@http@response:set_body(Resp, Body)};

                {error, _} ->
                    {error,
                        gleam@dynamic:from(
                            <<"Response body was not valid UTF-8"/utf8>>
                        )}
            end end
    ).

-spec send(gleam@http@request:request(binary())) -> {ok,
        gleam@http@response:response(binary())} |
    {error, gleam@dynamic:dynamic_()}.
send(Req) ->
    _pipe = configure(),
    dispatch(_pipe, Req).
