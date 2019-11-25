%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(els_poi).

%% Constructor
-export([ new/3
        , new/4
        ]).

-export([ match_pos/2
        , sort/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================

%% @edoc Constructor for a Point of Interest.
-spec new(pos(), poi_kind(), any()) -> poi().
new(Pos, Kind, Id) ->
  new(Pos, Kind, Id, undefined).

%% @edoc Constructor for a Point of Interest.
-spec new(pos(), poi_kind(), any(), any()) -> poi().
new(Pos, Kind, Id, Data) ->
  Range = els_range:range(Pos, Kind, Id),
  #{ kind  => Kind
   , id    => Id
   , data  => Data
   , range => Range
   }.

-spec match_pos([poi()], pos()) -> [poi()].
match_pos(POIs, Pos) ->
  [POI || #{range := #{ from := From
                      , to   := To
                      }} = POI <- POIs, (From =< Pos) andalso (Pos =< To)].

%% @doc Sorts pois based on their range
%%
%% Order is defined using els_range:compare/2.
-spec sort([poi()]) -> [poi()].
sort(POIs) ->
  lists:sort(fun compare/2, POIs).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec compare(poi(), poi()) -> boolean().
compare(#{range := A},  #{range := B}) ->
  els_range:compare(A, B).
