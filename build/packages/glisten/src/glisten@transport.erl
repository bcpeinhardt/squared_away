-module(glisten@transport).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([controlling_process/3, listen/3, accept_timeout/3, accept/2, handshake/2, receive_timeout/4, 'receive'/3, send/3, close/2, shutdown/2, set_opts/3, negotiated_protocol/2, decode_ip/0, peername/2, socket_info/1, get_socket_opts/3, set_buffer_size/2, sockname/2]).
-export_type([transport/0, ip_address/0]).

-type transport() :: tcp | ssl.

-type ip_address() :: {ip_v4, integer(), integer(), integer(), integer()} |
    {ip_v6,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-spec controlling_process(
    transport(),
    glisten@socket:socket(),
    gleam@erlang@process:pid_()
) -> {ok, nil} | {error, gleam@erlang@atom:atom_()}.
controlling_process(Transport, Socket, Pid) ->
    case Transport of
        tcp ->
            glisten_tcp_ffi:controlling_process(Socket, Pid);

        ssl ->
            glisten_ssl_ffi:controlling_process(Socket, Pid)
    end.

-spec listen(transport(), integer(), list(glisten@socket@options:tcp_option())) -> {ok,
        glisten@socket:listen_socket()} |
    {error, glisten@socket:socket_reason()}.
listen(Transport, Port, Opts) ->
    case Transport of
        tcp ->
            glisten@tcp:listen(Port, Opts);

        ssl ->
            glisten@ssl:listen(Port, Opts)
    end.

-spec accept_timeout(transport(), glisten@socket:listen_socket(), integer()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept_timeout(Transport, Socket, Timeout) ->
    case Transport of
        tcp ->
            gen_tcp:accept(Socket, Timeout);

        ssl ->
            ssl:transport_accept(Socket, Timeout)
    end.

-spec accept(transport(), glisten@socket:listen_socket()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept(Transport, Socket) ->
    case Transport of
        tcp ->
            gen_tcp:accept(Socket);

        ssl ->
            ssl:transport_accept(Socket)
    end.

-spec handshake(transport(), glisten@socket:socket()) -> {ok,
        glisten@socket:socket()} |
    {error, nil}.
handshake(Transport, Socket) ->
    case Transport of
        tcp ->
            glisten@tcp:handshake(Socket);

        ssl ->
            ssl:handshake(Socket)
    end.

-spec receive_timeout(
    transport(),
    glisten@socket:socket(),
    integer(),
    integer()
) -> {ok, bitstring()} | {error, glisten@socket:socket_reason()}.
receive_timeout(Transport, Socket, Amount, Timeout) ->
    case Transport of
        tcp ->
            gen_tcp:recv(Socket, Amount, Timeout);

        ssl ->
            ssl:recv(Socket, Amount, Timeout)
    end.

-spec 'receive'(transport(), glisten@socket:socket(), integer()) -> {ok,
        bitstring()} |
    {error, glisten@socket:socket_reason()}.
'receive'(Transport, Socket, Amount) ->
    case Transport of
        tcp ->
            gen_tcp:recv(Socket, Amount);

        ssl ->
            ssl:recv(Socket, Amount)
    end.

-spec send(
    transport(),
    glisten@socket:socket(),
    gleam@bytes_builder:bytes_builder()
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
send(Transport, Socket, Data) ->
    case Transport of
        tcp ->
            glisten_tcp_ffi:send(Socket, Data);

        ssl ->
            glisten_ssl_ffi:send(Socket, Data)
    end.

-spec close(transport(), glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
close(Transport, Socket) ->
    case Transport of
        tcp ->
            glisten_tcp_ffi:close(Socket);

        ssl ->
            glisten_ssl_ffi:close(Socket)
    end.

-spec shutdown(transport(), glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
shutdown(Transport, Socket) ->
    case Transport of
        tcp ->
            glisten@tcp:shutdown(Socket);

        ssl ->
            glisten@ssl:shutdown(Socket)
    end.

-spec set_opts(
    transport(),
    glisten@socket:socket(),
    list(glisten@socket@options:tcp_option())
) -> {ok, nil} | {error, nil}.
set_opts(Transport, Socket, Opts) ->
    case Transport of
        tcp ->
            glisten@tcp:set_opts(Socket, Opts);

        ssl ->
            glisten@ssl:set_opts(Socket, Opts)
    end.

-spec negotiated_protocol(transport(), glisten@socket:socket()) -> {ok,
        binary()} |
    {error, binary()}.
negotiated_protocol(Transport, Socket) ->
    case Transport of
        tcp ->
            {error, <<"Can't negotiate protocol on tcp"/utf8>>};

        ssl ->
            glisten_ssl_ffi:negotiated_protocol(Socket)
    end.

-spec decode_ipv4() -> fun((gleam@dynamic:dynamic_()) -> {ok, ip_address()} |
    {error, list(gleam@dynamic:decode_error())}).
decode_ipv4() ->
    gleam@dynamic:decode4(
        fun(Field@0, Field@1, Field@2, Field@3) -> {ip_v4, Field@0, Field@1, Field@2, Field@3} end,
        gleam@dynamic:element(0, fun gleam@dynamic:int/1),
        gleam@dynamic:element(1, fun gleam@dynamic:int/1),
        gleam@dynamic:element(2, fun gleam@dynamic:int/1),
        gleam@dynamic:element(3, fun gleam@dynamic:int/1)
    ).

-spec decode_ipv6() -> fun((gleam@dynamic:dynamic_()) -> {ok, ip_address()} |
    {error, list(gleam@dynamic:decode_error())}).
decode_ipv6() ->
    fun(Dyn) ->
        _pipe = (gleam@dynamic:decode8(
            fun(Field@0, Field@1, Field@2, Field@3, Field@4, Field@5, Field@6, Field@7) -> {ip_v6, Field@0, Field@1, Field@2, Field@3, Field@4, Field@5, Field@6, Field@7} end,
            gleam@dynamic:element(0, fun gleam@dynamic:int/1),
            gleam@dynamic:element(1, fun gleam@dynamic:int/1),
            gleam@dynamic:element(2, fun gleam@dynamic:int/1),
            gleam@dynamic:element(3, fun gleam@dynamic:int/1),
            gleam@dynamic:element(4, fun gleam@dynamic:int/1),
            gleam@dynamic:element(5, fun gleam@dynamic:int/1),
            gleam@dynamic:element(6, fun gleam@dynamic:int/1),
            gleam@dynamic:element(7, fun gleam@dynamic:int/1)
        ))(Dyn),
        gleam@result:then(_pipe, fun(Ip) -> case Ip of
                    {ip_v6, 0, 0, 0, 0, 0, 65535, A, B} ->
                        (decode_ipv4())(
                            inet:ipv4_mapped_ipv6_address(
                                {0, 0, 0, 0, 0, 65535, A, B}
                            )
                        );

                    _ ->
                        {ok, Ip}
                end end)
    end.

-spec decode_ip() -> fun((gleam@dynamic:dynamic_()) -> {ok, ip_address()} |
    {error, list(gleam@dynamic:decode_error())}).
decode_ip() ->
    gleam@dynamic:any([decode_ipv6(), decode_ipv4()]).

-spec peername(transport(), glisten@socket:socket()) -> {ok,
        {ip_address(), integer()}} |
    {error, nil}.
peername(Transport, Socket) ->
    _pipe = case Transport of
        tcp ->
            inet:peername(Socket);

        ssl ->
            ssl:peername(Socket)
    end,
    gleam@result:then(
        _pipe,
        fun(Pair) ->
            {Ip_address, Port} = Pair,
            _pipe@1 = (decode_ip())(Ip_address),
            _pipe@2 = gleam@result:map(_pipe@1, fun(Ip) -> {Ip, Port} end),
            gleam@result:nil_error(_pipe@2)
        end
    ).

-spec socket_info(glisten@socket:socket()) -> gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()).
socket_info(Socket) ->
    socket:info(Socket).

-spec get_socket_opts(
    transport(),
    glisten@socket:socket(),
    list(gleam@erlang@atom:atom_())
) -> {ok, list({gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()})} |
    {error, nil}.
get_socket_opts(Transport, Socket, Opts) ->
    case Transport of
        tcp ->
            inet:getopts(Socket, Opts);

        ssl ->
            ssl:getopts(Socket, Opts)
    end.

-spec set_buffer_size(transport(), glisten@socket:socket()) -> {ok, nil} |
    {error, nil}.
set_buffer_size(Transport, Socket) ->
    _pipe = get_socket_opts(
        Transport,
        Socket,
        [erlang:binary_to_atom(<<"recbuf"/utf8>>)]
    ),
    _pipe@1 = gleam@result:then(_pipe, fun(P) -> case P of
                [{_, Value}] ->
                    gleam@result:nil_error(gleam@dynamic:int(Value));

                _ ->
                    {error, nil}
            end end),
    gleam@result:then(
        _pipe@1,
        fun(Value@1) -> set_opts(Transport, Socket, [{buffer, Value@1}]) end
    ).

-spec sockname(transport(), glisten@socket:listen_socket()) -> {ok,
        {ip_address(), integer()}} |
    {error, glisten@socket:socket_reason()}.
sockname(Transport, Socket) ->
    _pipe = case Transport of
        tcp ->
            inet:sockname(Socket);

        ssl ->
            ssl:sockname(Socket)
    end,
    gleam@result:then(
        _pipe,
        fun(Pair) ->
            {Maybe_ip, Port} = Pair,
            _pipe@1 = (decode_ip())(Maybe_ip),
            _pipe@2 = gleam@result:map(_pipe@1, fun(Ip) -> {Ip, Port} end),
            gleam@result:replace_error(_pipe@2, badarg)
        end
    ).
