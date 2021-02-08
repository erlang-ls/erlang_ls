-module(els_dap_test_module).

-export([entry/1]).

-spec entry(non_neg_integer()) -> ok.
entry(0) ->
    ok;
entry(N) ->
    ds(),
    %% tail recursive call
    entry(N - 1).

-spec ds() -> ok.
ds() ->
    Atom = '42',
    Int = 42,
    Float = 42.0,
    Binary = <<"42">>,
    CharList = "42",
    Pid = self(),
    Ref = make_ref(),
    Tuple = {Binary, CharList},
    Map = #{
        Int => Float,
        Atom => Binary,
        CharList => Pid,
        Ref => Tuple
    },
    dummy(Map).

-spec dummy(map()) -> ok.
dummy(_) -> ok.
