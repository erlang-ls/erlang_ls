%%==============================================================================
%% The 'document' table
%%==============================================================================

-module(els_dt_document).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-export([ name/0
        , opts/0
        ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ insert/1
        , lookup/1
        , find_by/1
        , find_by_id/1
        , find_by_kind/1
        ]).

-export([ new/2
        , pois/1
        , pois/2
        , get_element_at_pos/3
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_document, { uri  :: uri()           | '_'
                         , id   :: any()           | '_'
                         , kind :: module | header | '_'
                         , text :: binary()        | '_'
                         , md5  :: binary()        | '_'
                         , pois :: [poi()]         | '_'
                         }).
-type els_dt_document() :: #els_dt_document{}.

-type item() :: #{ uri  := uri()
                 , id   := any()
                 , kind := module | header
                 , text := binary()
                 , md5  => binary()
                 , pois => [poi()]
                 }.
-export_type([ item/0 ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [ {attributes , record_info(fields, els_dt_document)}
  , {disc_copies, [node()]}
  , {index      , [#els_dt_document.id, #els_dt_document.kind]}
  , {type       , set}
  ].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_dt_document().
from_item(#{ uri  := Uri
           , id   := Id
           , kind := Kind
           , text := Text
           , md5  := MD5
           , pois := POIs
           }) ->
  #els_dt_document{ uri  = Uri
                  , id   = Id
                  , kind = Kind
                  , text = Text
                  , md5  = MD5
                  , pois = POIs
                  }.

-spec to_item(els_dt_document()) -> item().
to_item(#els_dt_document{ uri  = Uri
                        , id   = Id
                        , kind = Kind
                        , text = Text
                        , md5  = MD5
                        , pois = POIs
                        }) ->
  #{ uri  => Uri
   , id   => Id
   , kind => Kind
   , text => Text
   , md5  => MD5
   , pois => POIs
   }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_item(Map),
  els_db:write(Record).

-spec lookup(uri()) -> {ok, [item()]} | {error, any()}.
lookup(Uri) ->
  case els_db:lookup(name(), Uri) of
    {ok, Items} ->
      {ok, [to_item(Item) || Item <- Items]};
    {error, Reason} ->
      {error, Reason}
  end.

%% @edoc Find by id
-spec find_by_id(any()) -> {ok, [item()]} | {error, any()}.
find_by_id(Id) ->
  Pattern = #els_dt_document{id = Id, _ = '_'},
  find_by(Pattern).

%% @edoc Find by kind
-spec find_by_kind(any()) -> {ok, [item()]} | {error, any()}.
find_by_kind(Kind) ->
  Pattern = #els_dt_document{kind = Kind, _ = '_'},
  find_by(Pattern).

%% TODO: Improve efficiency
-spec find_by(tuple()) -> {ok, [item()]} | {error, any()}.
find_by(Pattern) ->
  case els_db:match(Pattern) of
    {ok, Items} ->
      {ok, [to_item(Item) || Item <- Items]};
    {error, Error} ->
      {error, Error}
  end.

-spec new(uri(), binary()) -> item().
new(Uri, Text) ->
  Extension = filename:extension(Uri),
  Id         = binary_to_atom(filename:basename(Uri, Extension), utf8),
  case Extension of
    <<".erl">> ->
      new(Uri, Text, Id, module);
    <<".hrl">> ->
      new(Uri, Text, Id, header)
  end.

-spec new(uri(), binary(), atom(), module | header) -> item().
new(Uri, Text, Id, Kind) ->
  {ok, POIs} = els_parser:parse(Text),
  MD5        = erlang:md5(Text),
  #{ uri  => Uri
   , id   => Id
   , kind => Kind
   , text => Text
   , md5  => MD5
   , pois => POIs
   }.

%% @edoc Returns the list of POIs for the current document
-spec pois(item()) -> [poi()].
pois(#{ pois := POIs }) ->
  POIs.

%% @edoc Returns the list of POIs of the given types for the current
%%       document
-spec pois(item(), [poi_kind()]) -> [poi()].
pois(Item, Kinds) ->
  [POI || #{kind := K} = POI <- pois(Item), lists:member(K, Kinds)].

-spec get_element_at_pos(item(), non_neg_integer(), non_neg_integer()) ->
  [any()].
get_element_at_pos(Item, Line, Column) ->
  POIs = maps:get(pois, Item),
  MatchedPOIs = els_poi:match_pos(POIs, {Line, Column}),
  els_poi:sort(MatchedPOIs).
