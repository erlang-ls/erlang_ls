%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(els_poi).

%% Constructor
-export([
    new/3,
    new/4
]).

-export([
    match_pos/2,
    sort/1,
    label/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_core.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type poi_kind() ::
    application
    | atom
    | behaviour
    | callback
    | define
    | export
    | export_entry
    | export_type
    | export_type_entry
    | folding_range
    | function
    | function_clause
    | implicit_fun
    | import_entry
    | include
    | include_lib
    | macro
    | module
    | parse_transform
    | record
    | record_def_field
    | record_expr
    | record_field
    | spec
    | type_application
    | type_definition
    | variable.
-type poi_range() :: #{from := pos(), to := pos()}.
-type poi_id() ::
    atom()
    %% record_def_field, record_field
    | {atom(), atom()}
    %% include, include_lib
    | string()
    | {atom(), arity()}
    | {module(), atom(), arity()}.
-type poi_data() :: any().
-type poi() :: #{
    kind := poi_kind(),
    id := poi_id(),
    data := poi_data(),
    range := poi_range()
}.
-export_type([poi/0, poi_range/0, poi_id/0, poi_kind/0]).

%%==============================================================================
%% Behaviour Definition
%%==============================================================================
-callback label(poi()) -> binary().

%%==============================================================================
%% API
%%==============================================================================

%% @doc Constructor for a Point of Interest.
-spec new(poi_range(), poi_kind(), any()) -> poi().
new(Range, Kind, Id) ->
    new(Range, Kind, Id, undefined).

%% @doc Constructor for a Point of Interest.
-spec new(poi_range(), poi_kind(), any(), any()) -> poi().
new(Range, Kind, Id, Data) ->
    #{
        kind => Kind,
        id => Id,
        data => Data,
        range => Range
    }.

-spec match_pos([poi()], pos()) -> [poi()].
match_pos(POIs, Pos) ->
    [
        POI
     || #{
            range := #{
                from := From,
                to := To
            }
        } = POI <- POIs,
        (From =< Pos) andalso (Pos =< To)
    ].

%% @doc Sorts pois based on their range
%%
%% Order is defined using els_range:compare/2.
-spec sort([poi()]) -> [poi()].
sort(POIs) ->
    lists:sort(fun compare/2, POIs).

-spec label(els_poi:poi()) -> binary().
label(#{kind := Kind} = POI) ->
    (callback_module(Kind)):label(POI).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec compare(poi(), poi()) -> boolean().
compare(#{range := A}, #{range := B}) ->
    els_range:compare(A, B).

-spec callback_module(poi_kind()) -> atom().
callback_module(function) ->
    els_poi_function.
