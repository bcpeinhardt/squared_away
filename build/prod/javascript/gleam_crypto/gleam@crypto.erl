-module(gleam@crypto).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([strong_random_bytes/1, hash/2, hmac/3, secure_compare/2, sign_message/3, verify_signed_message/2]).
-export_type([hash_algorithm/0]).

-type hash_algorithm() :: sha224 | sha256 | sha384 | sha512 | md5 | sha1.

-spec strong_random_bytes(integer()) -> bitstring().
strong_random_bytes(A) ->
    crypto:strong_rand_bytes(A).

-spec hash(hash_algorithm(), bitstring()) -> bitstring().
hash(A, B) ->
    gleam_crypto_ffi:hash(A, B).

-spec hmac(bitstring(), hash_algorithm(), bitstring()) -> bitstring().
hmac(Data, Algorithm, Key) ->
    gleam_crypto_ffi:hmac(Data, Algorithm, Key).

-spec do_secure_compare(bitstring(), bitstring(), integer()) -> boolean().
do_secure_compare(Left, Right, Accumulator) ->
    case {Left, Right} of
        {<<X, Left@1/binary>>, <<Y, Right@1/binary>>} ->
            Accumulator@1 = erlang:'bor'(Accumulator, erlang:'bxor'(X, Y)),
            do_secure_compare(Left@1, Right@1, Accumulator@1);

        {_, _} ->
            (Left =:= Right) andalso (Accumulator =:= 0)
    end.

-spec secure_compare(bitstring(), bitstring()) -> boolean().
secure_compare(Left, Right) ->
    case erlang:byte_size(Left) =:= erlang:byte_size(Right) of
        true ->
            do_secure_compare(Left, Right, 0);

        false ->
            false
    end.

-spec signing_input(hash_algorithm(), bitstring()) -> binary().
signing_input(Digest_type, Message) ->
    Protected = case Digest_type of
        sha224 ->
            <<"HS224"/utf8>>;

        sha256 ->
            <<"HS256"/utf8>>;

        sha384 ->
            <<"HS384"/utf8>>;

        sha512 ->
            <<"HS512"/utf8>>;

        sha1 ->
            <<"HS1"/utf8>>;

        md5 ->
            <<"HMD5"/utf8>>
    end,
    gleam@string:concat(
        [gleam@bit_array:base64_url_encode(<<Protected/binary>>, false),
            <<"."/utf8>>,
            gleam@bit_array:base64_url_encode(Message, false)]
    ).

-spec sign_message(bitstring(), bitstring(), hash_algorithm()) -> binary().
sign_message(Message, Secret, Digest_type) ->
    Input = signing_input(Digest_type, Message),
    Signature = gleam_crypto_ffi:hmac(<<Input/binary>>, Digest_type, Secret),
    gleam@string:concat(
        [Input,
            <<"."/utf8>>,
            gleam@bit_array:base64_url_encode(Signature, false)]
    ).

-spec verify_signed_message(binary(), bitstring()) -> {ok, bitstring()} |
    {error, nil}.
verify_signed_message(Message, Secret) ->
    gleam@result:then(case gleam@string:split(Message, <<"."/utf8>>) of
            [A, B, C] ->
                {ok, {A, B, C}};

            _ ->
                {error, nil}
        end, fun(_use0) ->
            {Protected, Payload, Signature} = _use0,
            Text = gleam@string:concat([Protected, <<"."/utf8>>, Payload]),
            gleam@result:then(
                gleam@bit_array:base64_url_decode(Payload),
                fun(Payload@1) ->
                    gleam@result:then(
                        gleam@bit_array:base64_url_decode(Signature),
                        fun(Signature@1) ->
                            gleam@result:then(
                                gleam@bit_array:base64_url_decode(Protected),
                                fun(Protected@1) ->
                                    gleam@result:then(case Protected@1 of
                                            <<72, 83, 50, 50, 52>> ->
                                                {ok, sha224};

                                            <<72, 83, 50, 53, 54>> ->
                                                {ok, sha256};

                                            <<72, 83, 51, 56, 52>> ->
                                                {ok, sha384};

                                            <<72, 83, 53, 49, 50>> ->
                                                {ok, sha512};

                                            <<72, 83, 49>> ->
                                                {ok, sha1};

                                            <<72, 77, 68, 53>> ->
                                                {ok, md5};

                                            _ ->
                                                {error, nil}
                                        end, fun(Digest_type) ->
                                            Challenge = gleam_crypto_ffi:hmac(
                                                <<Text/binary>>,
                                                Digest_type,
                                                Secret
                                            ),
                                            case secure_compare(
                                                Challenge,
                                                Signature@1
                                            ) of
                                                true ->
                                                    {ok, Payload@1};

                                                false ->
                                                    {error, nil}
                                            end
                                        end)
                                end
                            )
                        end
                    )
                end
            )
        end).
