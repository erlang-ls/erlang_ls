%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(erlang_ls_poi).

%% Constructor
-export([ new/4 ]).

%% Getters
-export([ data/1
        , kind/1
        , range/1
        ]).

-export([ match_pos/2 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================

%% @edoc Constructor for a Point of Interest.
-spec new(tree(), poi_kind(), any(), extra()) -> poi().
new(Tree, Kind, Data, Extra) ->
  Pos   = erl_syntax:get_pos(Tree),
  Range = erlang_ls_range:range(Pos, Kind, Data, Extra),
  #{ kind  => Kind
   , data  => Data
   , range => Range
   }.

-spec data(poi()) -> any().
data(#{ data := Data }) ->
  Data.

-spec kind(poi()) -> any().
kind(#{ kind := Kind }) ->
  Kind.

-spec range(poi()) -> any().
range(#{ range := Range }) ->
  Range.

-spec match_pos([poi()], pos()) -> [poi()].
match_pos(POIs, Pos) ->
  [POI || #{range := #{ from := From
                      , to    := To
                      }} = POI <- POIs, (From =< Pos) andalso (Pos =< To)].
