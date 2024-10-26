-module(glisten@ssl).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([controlling_process/2, accept_timeout/2, accept/1, receive_timeout/3, 'receive'/2, send/2, close/1, do_shutdown/2, shutdown/1, set_opts/2, handshake/1, listen/2, negotiated_protocol/1, peername/1, sockname/1, start/0, get_socket_opts/2]).

-spec controlling_process(glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok,
        nil} |
    {error, gleam@erlang@atom:atom_()}.
controlling_process(Socket, Pid) ->
    glisten_ssl_ffi:controlling_process(Socket, Pid).

-spec accept_timeout(glisten@socket:listen_socket(), integer()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept_timeout(Socket, Timeout) ->
    ssl:transport_accept(Socket, Timeout).

-spec accept(glisten@socket:listen_socket()) -> {ok, glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept(Socket) ->
    ssl:transport_accept(Socket).

-spec receive_timeout(glisten@socket:socket(), integer(), integer()) -> {ok,
        bitstring()} |
    {error, glisten@socket:socket_reason()}.
receive_timeout(Socket, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout).

-spec 'receive'(glisten@socket:socket(), integer()) -> {ok, bitstring()} |
    {error, glisten@socket:socket_reason()}.
'receive'(Socket, Length) ->
    ssl:recv(Socket, Length).

-spec send(glisten@socket:socket(), gleam@bytes_builder:bytes_builder()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
send(Socket, Packet) ->
    glisten_ssl_ffi:send(Socket, Packet).

-spec close(glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
close(Socket) ->
    glisten_ssl_ffi:close(Socket).

-spec do_shutdown(glisten@socket:socket(), gleam@erlang@atom:atom_()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
do_shutdown(Socket, Write) ->
    glisten_ssl_ffi:shutdown(Socket, Write).

-spec shutdown(glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
shutdown(Socket) ->
    _assert_subject = gleam_erlang_ffi:atom_from_string(<<"write"/utf8>>),
    {ok, Write} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"glisten/ssl"/utf8>>,
                        function => <<"shutdown"/utf8>>,
                        line => 48})
    end,
    glisten_ssl_ffi:shutdown(Socket, Write).

-spec set_opts(
    glisten@socket:socket(),
    list(glisten@socket@options:tcp_option())
) -> {ok, nil} | {error, nil}.
set_opts(Socket, Opts) ->
    _pipe = Opts,
    _pipe@1 = glisten@socket@options:to_dict(_pipe),
    _pipe@2 = maps:to_list(_pipe@1),
    _pipe@3 = gleam@list:map(_pipe@2, fun gleam@dynamic:from/1),
    glisten_ssl_ffi:set_opts(Socket, _pipe@3).

-spec handshake(glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
    {error, nil}.
handshake(Socket) ->
    ssl:handshake(Socket).

-spec listen(integer(), list(glisten@socket@options:tcp_option())) -> {ok,
        glisten@socket:listen_socket()} |
    {error, glisten@socket:socket_reason()}.
listen(Port, Options) ->
    _pipe = Options,
    _pipe@1 = glisten@socket@options:merge_with_defaults(_pipe),
    ssl:listen(Port, _pipe@1).

-spec negotiated_protocol(glisten@socket:socket()) -> {ok, binary()} |
    {error, binary()}.
negotiated_protocol(Socket) ->
    glisten_ssl_ffi:negotiated_protocol(Socket).

-spec peername(glisten@socket:socket()) -> {ok,
        {gleam@dynamic:dynamic_(), integer()}} |
    {error, nil}.
peername(Socket) ->
    ssl:peername(Socket).

-spec sockname(glisten@socket:listen_socket()) -> {ok,
        {gleam@dynamic:dynamic_(), integer()}} |
    {error, glisten@socket:socket_reason()}.
sockname(Socket) ->
    ssl:sockname(Socket).

-spec start() -> {ok, nil} | {error, gleam@dynamic:dynamic_()}.
start() ->
    glisten_ssl_ffi:start_ssl().

-spec get_socket_opts(glisten@socket:socket(), list(gleam@erlang@atom:atom_())) -> {ok,
        list({gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()})} |
    {error, nil}.
get_socket_opts(Socket, Opts) ->
    ssl:getopts(Socket, Opts).
