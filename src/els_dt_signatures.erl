%%==============================================================================
%% The 'signatures' table
%%==============================================================================

-module(els_dt_signatures).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-behaviour(els_db_table).
-export([ name/0
        , opts/0
        ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ insert/1
        , lookup/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_signatures, { mfa  :: mfa()  | '_'
                           , tree :: tree()
                           }).
-type els_dt_signatures() :: #els_dt_signatures{}.

-type item() :: #{ mfa      := mfa()
                 , tree := tree()
                 }.
-export_type([ item/0 ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [ {attributes , record_info(fields, els_dt_signatures)}
  , {disc_copies, [node()]}
  , {index      , []}
  , {type       , set}
  ].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_dt_signatures().
from_item(#{ mfa := MFA, tree := Tree }) ->
  #els_dt_signatures{ mfa = MFA, tree = Tree }.

-spec to_item(els_dt_signatures()) -> item().
to_item(#els_dt_signatures{ mfa = MFA, tree = Tree }) ->
  #{ mfa  => MFA
   , tree => Tree
   }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_item(Map),
  els_db:write(Record).

-spec lookup(mfa()) -> {ok, [item()]} | {error, any()}.
lookup(MFA) ->
  case els_db:lookup(name(), MFA) of
    {ok, Items} ->
      {ok, [to_item(Item) || Item <- Items]};
    {error, Reason} ->
      {error, Reason}
  end.
