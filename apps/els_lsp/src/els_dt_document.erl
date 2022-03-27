%%==============================================================================
%% The 'document' table
%%==============================================================================

-module(els_dt_document).

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
        , delete/1
        ]).

-export([ new/2
        , pois/1
        , pois/2
        , get_element_at_pos/3
        , uri/1
        , functions_at_pos/3
        , applications_at_pos/3
        , wrapping_functions/2
        , wrapping_functions/3
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type id()   :: atom().
-type kind() :: module | header | other.

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_document, { uri  :: uri()    | '_'
                         , id   :: id()     | '_'
                         , kind :: kind()   | '_'
                         , text :: binary() | '_'
                         , md5  :: binary() | '_'
                         , pois :: [poi()]  | '_'
                         }).
-type els_dt_document() :: #els_dt_document{}.

-type item() :: #{ uri  := uri()
                 , id   := id()
                 , kind := kind()
                 , text := binary()
                 , md5  => binary()
                 , pois => [poi()]
                 }.
-export_type([ id/0
             , item/0
             , kind/0
             ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [set, compressed].

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
  els_db:write(name(), Record).

-spec lookup(uri()) -> {ok, [item()]}.
lookup(Uri) ->
  {ok, Items} = els_db:lookup(name(), Uri),
  {ok, [to_item(Item) || Item <- Items]}.

-spec delete(uri()) -> ok.
delete(Uri) ->
  els_db:delete(name(), Uri).

-spec new(uri(), binary()) -> item().
new(Uri, Text) ->
  Extension = filename:extension(Uri),
  Id = binary_to_atom(filename:basename(Uri, Extension), utf8),
  case Extension of
    <<".erl">> ->
      new(Uri, Text, Id, module);
    <<".hrl">> ->
      new(Uri, Text, Id, header);
    _  ->
      new(Uri, Text, Id, other)
  end.

-spec new(uri(), binary(), atom(), kind()) -> item().
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

%% @doc Returns the list of POIs for the current document
-spec pois(item()) -> [poi()].
pois(#{ pois := POIs }) ->
  POIs.

%% @doc Returns the list of POIs of the given types for the current
%%      document
-spec pois(item(), [poi_kind()]) -> [poi()].
pois(Item, Kinds) ->
  [POI || #{kind := K} = POI <- pois(Item), lists:member(K, Kinds)].

-spec get_element_at_pos(item(), non_neg_integer(), non_neg_integer()) ->
  [poi()].
get_element_at_pos(Item, Line, Column) ->
  POIs = pois(Item),
  MatchedPOIs = els_poi:match_pos(POIs, {Line, Column}),
  els_poi:sort(MatchedPOIs).

%% @doc Returns the URI of the current document
-spec uri(item()) -> uri().
uri(#{ uri := Uri }) ->
  Uri.

-spec functions_at_pos(item(), non_neg_integer(), non_neg_integer()) -> [poi()].
functions_at_pos(Item, Line, Column) ->
  POIs = get_element_at_pos(Item, Line, Column),
  [POI || #{kind := 'function'} = POI <- POIs].

-spec applications_at_pos(item(), non_neg_integer(), non_neg_integer()) ->
        [poi()].
applications_at_pos(Item, Line, Column) ->
  POIs = get_element_at_pos(Item, Line, Column),
  [POI || #{kind := 'application'} = POI <- POIs].

-spec wrapping_functions(item(), non_neg_integer(), non_neg_integer()) ->
        [poi()].
wrapping_functions(Document, Line, Column) ->
  Range = #{from => {Line, Column}, to => {Line, Column}},
  Functions = pois(Document, ['function']),
  [F || #{data := #{ wrapping_range := WR}} = F <- Functions,
        els_range:in(Range, WR)].

-spec wrapping_functions(item(), range()) -> [poi()].
wrapping_functions(Document, Range) ->
  #{start := #{character := Character, line := Line}} = Range,
  wrapping_functions(Document, Line, Character).
