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
        , find_by_id/1
        , insert/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

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
  [ {attributes        , record_info(fields, els_dt_references)}
  , {disc_copies       , [node()]}
  , {index             , [#els_dt_references.uri]}
  , {type              , bag}
  , {storage_properties, [{ets, [ {read_concurrency, true}
                                , {write_concurrency, true} ]}]}
  ].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_dt_references().
from_item(#{ id := Id, uri := Uri, range := Range}) ->
  #els_dt_references{ id = Id, uri = Uri, range = Range}.

-spec to_item(els_dt_references()) -> item().
to_item(#els_dt_references{ id = Id, uri = Uri, range = Range }) ->
  #{ id    => Id
   , uri   => Uri
   , range => Range
   }.

-spec delete_by_uri(uri()) -> ok | {error, any()}.
delete_by_uri(Uri) ->
  Pattern = #els_dt_references{uri = Uri, _ = '_'},
  {ok, Items} = els_db:match(Pattern),
  [ok = els_db:delete_object(Item) ||  Item <- Items],
  ok.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_item(Map),
  els_db:write(Record).

%% @doc Find all
-spec find_all() -> {ok, [item()]} | {error, any()}.
find_all() ->
  Pattern = #els_dt_references{_ = '_'},
  find_by(Pattern).

%% @doc Find by id
-spec find_by_id(any()) -> {ok, [item()]} | {error, any()}.
find_by_id(Id) ->
  Pattern = #els_dt_references{id = Id, _ = '_'},
  find_by(Pattern).

-spec find_by(tuple()) -> {ok, [item()]}.
find_by(Pattern) ->
  {ok, Items} = els_db:match(Pattern),
  {ok, [to_item(Item) || Item <- Items]}.
