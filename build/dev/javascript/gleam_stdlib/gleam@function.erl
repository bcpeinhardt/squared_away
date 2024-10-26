-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EUZ) -> EVA), fun((EVA) -> EVB)) -> fun((EUZ) -> EVB).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EVC, EVD) -> EVE)) -> fun((EVC) -> fun((EVD) -> EVE)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EVG, EVH, EVI) -> EVJ)) -> fun((EVG) -> fun((EVH) -> fun((EVI) -> EVJ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EVL, EVM, EVN, EVO) -> EVP)) -> fun((EVL) -> fun((EVM) -> fun((EVN) -> fun((EVO) -> EVP)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EVR, EVS, EVT, EVU, EVV) -> EVW)) -> fun((EVR) -> fun((EVS) -> fun((EVT) -> fun((EVU) -> fun((EVV) -> EVW))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EVY, EVZ, EWA, EWB, EWC, EWD) -> EWE)) -> fun((EVY) -> fun((EVZ) -> fun((EWA) -> fun((EWB) -> fun((EWC) -> fun((EWD) -> EWE)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EWG, EWH) -> EWI)) -> fun((EWH, EWG) -> EWI).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EWJ) -> EWJ.
identity(X) ->
    X.

-spec constant(EWK) -> fun((any()) -> EWK).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EWM, fun((EWM) -> any())) -> EWM.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EWO) -> EWP), EWO) -> EWP.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EWQ, EWR) -> EWS), EWQ, EWR) -> EWS.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EWT, EWU, EWV) -> EWW), EWT, EWU, EWV) -> EWW.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
