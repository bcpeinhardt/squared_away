-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EVL) -> EVM), fun((EVM) -> EVN)) -> fun((EVL) -> EVN).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EVO, EVP) -> EVQ)) -> fun((EVO) -> fun((EVP) -> EVQ)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EVS, EVT, EVU) -> EVV)) -> fun((EVS) -> fun((EVT) -> fun((EVU) -> EVV))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EVX, EVY, EVZ, EWA) -> EWB)) -> fun((EVX) -> fun((EVY) -> fun((EVZ) -> fun((EWA) -> EWB)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EWD, EWE, EWF, EWG, EWH) -> EWI)) -> fun((EWD) -> fun((EWE) -> fun((EWF) -> fun((EWG) -> fun((EWH) -> EWI))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EWK, EWL, EWM, EWN, EWO, EWP) -> EWQ)) -> fun((EWK) -> fun((EWL) -> fun((EWM) -> fun((EWN) -> fun((EWO) -> fun((EWP) -> EWQ)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EWS, EWT) -> EWU)) -> fun((EWT, EWS) -> EWU).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EWV) -> EWV.
identity(X) ->
    X.

-spec constant(EWW) -> fun((any()) -> EWW).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EWY, fun((EWY) -> any())) -> EWY.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EXA) -> EXB), EXA) -> EXB.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EXC, EXD) -> EXE), EXC, EXD) -> EXE.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EXF, EXG, EXH) -> EXI), EXF, EXG, EXH) -> EXI.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
