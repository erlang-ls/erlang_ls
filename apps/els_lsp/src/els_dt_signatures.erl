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
        , delete_by_module/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_signatures, { mfa  :: mfa()  | '_' | {atom(), '_', '_'}
                           , spec :: binary() | '_'
                           }).
-type els_dt_signatures() :: #els_dt_signatures{}.

-type item() :: #{ mfa  := mfa()
                 , spec := binary()
                 }.
-export_type([ item/0 ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [set].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_dt_signatures().
from_item(#{ mfa := MFA, spec := Spec }) ->
  #els_dt_signatures{ mfa = MFA, spec = Spec }.

-spec to_item(els_dt_signatures()) -> item().
to_item(#els_dt_signatures{ mfa = MFA, spec = Spec }) ->
  #{ mfa  => MFA
   , spec => Spec
   }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_item(Map),
  els_db:write(name(), Record).

-spec lookup(mfa()) -> {ok, [item()]}.
lookup({M, _F, _A} = MFA) ->
  case els_db:lookup(name(), MFA) of
    {ok, []} ->
      ok = index_signatures(M),
      els_db:lookup(name(), MFA);
    {ok, Items} ->
      {ok, [to_item(Item) || Item <- Items]}
  end.

-spec delete_by_module(atom()) -> ok.
delete_by_module(Module) ->
  Pattern = #els_dt_signatures{mfa = {Module, '_', '_'}, _ = '_'},
  ok = els_db:match_delete(name(), Pattern).

-spec index_signatures(atom()) -> ok.
index_signatures(M) ->
  case els_utils:find_module(M) of
    {ok, Uri} ->
      {ok, #{text := Text} = Document} = els_utils:lookup_document(Uri),
      POIs = els_dt_document:pois(Document, [spec]),
      [index_signature(M, Text, POI) || POI <- POIs];
    {error, Error} ->
      ?LOG_DEBUG("[~p] Cannot find module. [module=~p] [error=~p]",
                 [?MODULE, M, Error])
  end,
  ok.

-spec index_signature(atom(), binary(), poi()) -> ok.
index_signature(M, Text, POI) ->
  #{id := {F, A}, range := Range} = POI,
  #{from := From, to := To} = Range,
  Spec = els_text:range(Text, From, To),
  insert(#{ mfa => {M, F, A} , spec => Spec}).
