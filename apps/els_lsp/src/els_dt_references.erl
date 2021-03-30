%%==============================================================================
%% The 'references' table
%%==============================================================================

-module(els_dt_references).

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

-export([ delete_by_uri/1
        , find_all/0
        , find_by/1
        , find_by_id/2
        , insert/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_references, { id    :: any()       | '_'
                           , uri   :: uri()       | '_'
                           , range :: poi_range() | '_'
                           }).
-type els_dt_references() :: #els_dt_references{}.

-type item() :: #{ id    := any()
                 , uri   := uri()
                 , range := poi_range()
                 }.
-export_type([ item/0 ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [bag].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(poi_kind(), item()) -> els_dt_references().
from_item(Kind, #{ id := Id, uri := Uri, range := Range}) ->
  InternalId = {kind_to_category(Kind), Id},
  #els_dt_references{ id = InternalId, uri = Uri, range = Range}.

-spec to_item(els_dt_references()) -> item().
to_item(#els_dt_references{ id = {_Category, Id}, uri = Uri, range = Range }) ->
  #{ id    => Id
   , uri   => Uri
   , range => Range
   }.

-spec delete_by_uri(uri()) -> ok | {error, any()}.
delete_by_uri(Uri) ->
  Pattern = #els_dt_references{uri = Uri, _ = '_'},
  ok = els_db:match_delete(name(), Pattern).

-spec insert(poi_kind(), item()) -> ok | {error, any()}.
insert(Kind, Map) when is_map(Map) ->
  Record = from_item(Kind, Map),
  els_db:write(name(), Record).

%% @doc Find all
-spec find_all() -> {ok, [item()]} | {error, any()}.
find_all() ->
  Pattern = #els_dt_references{_ = '_'},
  find_by(Pattern).

%% @doc Find by id
-spec find_by_id(poi_kind(), any()) -> {ok, [item()]} | {error, any()}.
find_by_id(Kind, Id) ->
  InternalId = {kind_to_category(Kind), Id},
  Pattern = #els_dt_references{id = InternalId, _ = '_'},
  find_by(Pattern).

-spec find_by(tuple()) -> {ok, [item()]}.
find_by(Pattern) ->
  {ok, Items} = els_db:match(name(), Pattern),
  {ok, [to_item(Item) || Item <- Items]}.

-spec kind_to_category(poi_kind()) -> function | type | macro | record.
kind_to_category(Kind) when Kind =:= application;
                            Kind =:= export_entry;
                            Kind =:= function;
                            Kind =:= function_clause;
                            Kind =:= import_entry;
                            Kind =:= implicit_fun ->
  function;
kind_to_category(Kind) when Kind =:= export_type_entry;
                            Kind =:= type_application;
                            Kind =:= type_definition ->
  type;
kind_to_category(Kind) when Kind =:= macro;
                            Kind =:= define ->
  macro;
kind_to_category(Kind) when Kind =:= record_expr;
                            Kind =:= record ->
  record;
kind_to_category(Kind) when Kind =:= behaviour ->
  behaviour.
