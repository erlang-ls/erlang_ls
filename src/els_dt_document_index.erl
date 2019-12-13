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

-export([new/3]).

-export([ find_by_kind/1
        , insert/1
        , lookup/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

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
  [ {attributes        , record_info(fields, els_dt_document_index)}
  , {disc_copies       , [node()]}
  , {index             , [#els_dt_document_index.kind]}
  , {type              , bag}
  , {storage_properties, [{ets, [compressed]}]}
  ].

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
  els_db:write(Record).

-spec lookup(atom()) -> {ok, [item()]}.
lookup(Id) ->
  {ok, Items} = els_db:lookup(name(), Id),
  {ok, [to_item(Item) || Item <- Items]}.

-spec find_by_kind(els_dt_document:kind()) -> {ok, [item()]}.
find_by_kind(Kind) ->
  Pattern = #els_dt_document_index{kind = Kind, _ = '_'},
  {ok, Items} = els_db:match(Pattern),
  {ok, [to_item(Item) || Item <- Items]}.

-spec new(atom(), uri(), els_dt_document:kind()) -> item().
new(Id, Uri, Kind) ->
  #{ id   => Id
   , uri  => Uri
   , kind => Kind
   }.
