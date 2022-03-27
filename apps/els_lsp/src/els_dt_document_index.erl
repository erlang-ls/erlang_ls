%%==============================================================================
%% The 'document_index' table
%%==============================================================================

-module(els_dt_document_index).

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

-export([ new/3 ]).

-export([ find_by_kind/1
        , insert/1
        , lookup/1
        , delete_by_uri/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_document_index, { id   :: els_dt_document:id()   | '_'
                               , uri  :: uri()                  | '_'
                               , kind :: els_dt_document:kind() | '_'
                               }).
-type els_dt_document_index() :: #els_dt_document_index{}.

-type item() :: #{ id   := els_dt_document:id()
                 , uri  := uri()
                 , kind := els_dt_document:kind()
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

-spec from_item(item()) -> els_dt_document_index().
from_item(#{id := Id, uri := Uri, kind := Kind}) ->
  #els_dt_document_index{id = Id, uri = Uri, kind = Kind}.

-spec to_item(els_dt_document_index()) -> item().
to_item(#els_dt_document_index{id = Id, uri = Uri, kind = Kind}) ->
  #{id => Id, uri => Uri, kind => Kind}.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_item(Map),
  els_db:write(name(), Record).

-spec lookup(atom()) -> {ok, [item()]}.
lookup(Id) ->
  {ok, Items} = els_db:lookup(name(), Id),
  {ok, [to_item(Item) || Item <- Items]}.

-spec delete_by_uri(uri()) -> ok | {error, any()}.
delete_by_uri(Uri) ->
  Pattern = #els_dt_document_index{uri = Uri, _ = '_'},
  ok = els_db:match_delete(name(), Pattern).

-spec find_by_kind(els_dt_document:kind()) -> {ok, [item()]}.
find_by_kind(Kind) ->
  Pattern = #els_dt_document_index{kind = Kind, _ = '_'},
  {ok, Items} = els_db:match(name(), Pattern),
  {ok, [to_item(Item) || Item <- Items]}.

-spec new(atom(), uri(), els_dt_document:kind()) -> item().
new(Id, Uri, Kind) ->
  #{ id   => Id
   , uri  => Uri
   , kind => Kind
   }.
