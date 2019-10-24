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
        , tree/1
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
                     , tree := erlang_ls_tree:tree()
                     , pois := [erlang_ls_poi:poi()]
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
  {ok, Tree, Extra} = erlang_ls_parser:parse(Text),
  AnnotatedTree = erlang_ls_tree:annotate(Tree, Extra),
  POIs = erlang_ls_poi:list(AnnotatedTree),
  #{ uri  => Uri
   , text => Text
   , tree => AnnotatedTree
   , pois => POIs
   }.

-spec uri(document()) -> uri().
uri(#{uri := Uri}) ->
  Uri.

-spec text(document()) -> binary().
text(#{text := Text}) ->
  Text.

-spec tree(document()) -> erlang_ls_tree:tree().
tree(#{tree := Tree}) ->
  Tree.

-spec points_of_interest(document()) -> [erlang_ls_poi:poi()].
points_of_interest(Document) ->
  points_of_interest(Document, []).

-spec points_of_interest(document(), [erlang_ls_poi:kind()]) -> [erlang_ls_poi:poi()].
points_of_interest(Document, Kinds) ->
  points_of_interest(Document, Kinds, undefined).

-spec points_of_interest(document(), [erlang_ls_poi:kind()], any()) -> [erlang_ls_poi:poi()].
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
  AnnotatedTree = maps:get(tree, Document),
  %% TODO: Refine API
  erlang_ls_poi:match_pos(AnnotatedTree, {Line, Column}).
