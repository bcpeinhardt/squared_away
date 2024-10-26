-module(term_size_ffi).
-export([terminal_size/0]).

terminal_size() ->
    case {io:rows(), io:columns()} of
        {{ok, Rows}, {ok, Cols}} -> {ok, {Rows, Cols}};
        _ -> {error, nil}
    end.
