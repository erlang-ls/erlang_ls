%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(erlang_ls_poi).

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
  Pos = erl_syntax:get_pos(Tree),
  Range = erlang_ls_range:range(Pos, Kind, Data, Extra),
  #{ kind  => Kind
   , data  => Data
   , range => Range
   }.

-spec match_pos([poi()], pos()) -> [poi()].
match_pos(POIs, Pos) ->
  [POI || #{range := Range} = POI <- POIs, matches_pos(Pos, Range)].

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec matches_pos(pos(), poi_range()) -> boolean().
matches_pos(Pos, #{from := From, to := To}) ->
  (From =< Pos) andalso (Pos =< To).
