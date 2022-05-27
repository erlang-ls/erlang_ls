-module(els_poi_function).

-behaviour(els_poi).
-export([label/1, symbol_kind/0]).

-include("els_lsp.hrl").

-opaque id() :: {atom(), arity()}.
-export_type([id/0]).

-spec label(els_poi:poi()) -> binary().
label(#{id := {F, A}}) ->
    els_utils:to_binary(io_lib:format("~s/~p", [F, A])).

-spec symbol_kind() -> ?SYMBOLKIND_FUNCTION.
symbol_kind() ->
    ?SYMBOLKIND_FUNCTION.
