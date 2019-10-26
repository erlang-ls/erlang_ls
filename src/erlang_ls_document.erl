%%==============================================================================
%% Document gen_server
%%==============================================================================
-module(erlang_ls_document).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ create/2
        , uri/1
        , text/1
        , points_of_interest/1
        , points_of_interest/2
        , points_of_interest/3
        , get_element_at_pos/3
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type document() :: #{ uri  := uri()
                     , text := binary()
                     , pois := [poi()]
                     }.

-export_type([document/0]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

-spec create(uri(), binary()) -> document().
create(Uri, Text) ->
  {ok, POIs} = erlang_ls_parser:parse(Text),
  #{ uri  => Uri
   , text => Text
   , pois => POIs
   }.

-spec uri(document()) -> uri().
uri(#{uri := Uri}) ->
  Uri.

-spec text(document()) -> binary().
text(#{text := Text}) ->
  Text.

-spec points_of_interest(document()) -> [poi()].
points_of_interest(Document) ->
  points_of_interest(Document, []).

-spec points_of_interest(document(), [poi_kind()]) -> [poi()].
points_of_interest(Document, Kinds) ->
  points_of_interest(Document, Kinds, undefined).

-spec points_of_interest(document(), [poi_kind()], any()) -> [poi()].
points_of_interest(#{pois := POIs}, Kinds, undefined) ->
  [POI || #{ kind := Kind } = POI <- POIs, lists:member(Kind, Kinds)];
points_of_interest(#{pois := POIs}, Kinds, Pattern) ->
  [POI || #{ kind := Kind, data := Data } = POI <- POIs
            , lists:member(Kind, Kinds)
            , Pattern =:= Data
  ].

-spec get_element_at_pos(document(), non_neg_integer(), non_neg_integer()) ->
  [any()].
get_element_at_pos(Document, Line, Column) ->
  POIs = maps:get(pois, Document),
  erlang_ls_poi:match_pos(POIs, {Line, Column}).
