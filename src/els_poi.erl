%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(els_poi).

%% Constructor
-export([ new/4 ]).

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
  Range = els_range:range(Pos, Kind, Data, Extra),
  #{ kind  => Kind
   , data  => Data
   , range => Range
   , tree  => Tree
   }.

-spec match_pos([poi()], pos()) -> [poi()].
match_pos(POIs, Pos) ->
  [POI || #{range := #{ from := From
                      , to    := To
                      }} = POI <- POIs, (From =< Pos) andalso (Pos =< To)].
