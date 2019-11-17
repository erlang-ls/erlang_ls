%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(els_poi).

%% Constructor
-export([ new/3
        , new/4
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
                      , to    := To
                      }} = POI <- POIs, (From =< Pos) andalso (Pos =< To)].
