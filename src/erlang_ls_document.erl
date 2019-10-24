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
points_of_interest(#{pois := POIs}) ->
  POIs.

-spec points_of_interest(document(), [erlang_ls_poi:kind()]) -> [erlang_ls_poi:poi()].
points_of_interest(#{pois := POIs}, Kinds) ->
  [POI || #{ kind := Kind } = POI <- POIs, lists:member(Kind, Kinds)].

-spec get_element_at_pos(document(), non_neg_integer(), non_neg_integer()) ->
  [any()].
get_element_at_pos(Document, Line, Column) ->
  AnnotatedTree = maps:get(tree, Document),
  erlang_ls_poi:match_pos(AnnotatedTree, {Line, Column}).
