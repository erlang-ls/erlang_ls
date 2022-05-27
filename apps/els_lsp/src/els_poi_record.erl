-module(els_poi_record).

-behaviour(els_poi).
-export([label/1, symbol_kind/0]).

-include("els_lsp.hrl").

-opaque id() :: {atom(), arity()}.
-export_type([id/0]).

-spec label(els_poi:poi()) -> binary().
label(#{id := Name}) when is_atom(Name) ->
    atom_to_binary(Name, utf8).

-spec symbol_kind() -> ?SYMBOLKIND_STRUCT.
symbol_kind() ->
    ?SYMBOLKIND_STRUCT.
