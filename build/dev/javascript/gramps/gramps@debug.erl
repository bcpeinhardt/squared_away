-module(gramps@debug).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([literal_bits/2]).

-spec literal_bits(bitstring(), list(integer())) -> list(integer()).
literal_bits(Source, Values) ->
    case Source of
        <<>> ->
            lists:reverse(Values);

        <<Bit:1/integer, Rest/bitstring>> ->
            literal_bits(Rest, [Bit | Values]);

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"where'd that bit go"/utf8>>,
                    module => <<"gramps/debug"/utf8>>,
                    function => <<"literal_bits"/utf8>>,
                    line => 7})
    end.
