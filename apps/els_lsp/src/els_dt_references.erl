%%==============================================================================
%% The 'references' table
%%==============================================================================

-module(els_dt_references).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-behaviour(els_db_table).
-export([
    name/0,
    opts/0
]).

%%==============================================================================
%% API
%%==============================================================================

-export([
    delete_by_uri/1,
    versioned_delete_by_uri/2,
    find_by/1,
    find_by_id/2,
    insert/2,
    versioned_insert/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_references, {
    id :: any() | '_',
    uri :: uri() | '_',
    range :: els_poi:poi_range() | '_',
    version :: version() | '_'
}).
-type els_dt_references() :: #els_dt_references{}.
-type version() :: null | integer().
-type item() :: #{
    id := any(),
    uri := uri(),
    range := els_poi:poi_range(),
    version := version()
}.
-export_type([item/0]).

-type poi_category() ::
    function
    | type
    | macro
    | record
    | include
    | include_lib
    | behaviour.
-export_type([poi_category/0]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
    [bag].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(els_poi:poi_kind(), item()) -> els_dt_references().
from_item(Kind, #{
    id := Id,
    uri := Uri,
    range := Range,
    version := Version
}) ->
    InternalId = {kind_to_category(Kind), Id},
    #els_dt_references{
        id = InternalId,
        uri = Uri,
        range = Range,
        version = Version
    }.

-spec to_item(els_dt_references()) -> item().
to_item(#els_dt_references{
    id = {_Category, Id},
    uri = Uri,
    range = Range,
    version = Version
}) ->
    #{
        id => Id,
        uri => Uri,
        range => Range,
        version => Version
    }.

-spec delete_by_uri(uri()) -> ok | {error, any()}.
delete_by_uri(Uri) ->
    Pattern = #els_dt_references{uri = Uri, _ = '_'},
    ok = els_db:match_delete(name(), Pattern).

-spec versioned_delete_by_uri(uri(), version()) -> ok.
versioned_delete_by_uri(Uri, Version) ->
    MS = ets:fun2ms(fun
        (#els_dt_references{uri = U, version = CurrentVersion}) when
            U =:= Uri,
            CurrentVersion =:= null orelse
                CurrentVersion =< Version
        ->
            true;
        (_) ->
            false
    end),
    ok = els_db:select_delete(name(), MS).

-spec insert(els_poi:poi_kind(), item()) -> ok | {error, any()}.
insert(Kind, Map) when is_map(Map) ->
    Record = from_item(Kind, Map),
    els_db:write(name(), Record).

-spec versioned_insert(els_poi:poi_kind(), item()) -> ok | {error, any()}.
versioned_insert(Kind, #{id := Id, version := Version} = Map) ->
    Record = from_item(Kind, Map),
    Condition = fun(#els_dt_references{version = CurrentVersion}) ->
        CurrentVersion =:= null orelse Version >= CurrentVersion
    end,
    els_db:conditional_write(name(), Id, Record, Condition).

%% @doc Find by id
-spec find_by_id(els_poi:poi_kind(), any()) -> {ok, [item()]} | {error, any()}.
find_by_id(Kind, Id) ->
    InternalId = {kind_to_category(Kind), Id},
    Pattern = #els_dt_references{id = InternalId, _ = '_'},
    find_by(Pattern).

-spec find_by(tuple()) -> {ok, [item()]}.
find_by(#els_dt_references{id = Id} = Pattern) ->
    Uris = els_text_search:find_candidate_uris(Id),
    [els_indexing:ensure_deeply_indexed(Uri) || Uri <- Uris],
    {ok, Items} = els_db:match(name(), Pattern),
    {ok, [to_item(Item) || Item <- Items]}.

-spec kind_to_category(els_poi:poi_kind()) -> poi_category().
kind_to_category(Kind) when
    Kind =:= application;
    Kind =:= export_entry;
    Kind =:= function;
    Kind =:= function_clause;
    Kind =:= import_entry;
    Kind =:= implicit_fun
->
    function;
kind_to_category(Kind) when
    Kind =:= export_type_entry;
    Kind =:= type_application;
    Kind =:= type_definition
->
    type;
kind_to_category(Kind) when
    Kind =:= macro;
    Kind =:= define
->
    macro;
kind_to_category(Kind) when
    Kind =:= record_expr;
    Kind =:= record
->
    record;
kind_to_category(Kind) when
    Kind =:= record_def_field;
    Kind =:= record_field
->
    record_field;
kind_to_category(Kind) when Kind =:= include ->
    include;
kind_to_category(Kind) when Kind =:= include_lib ->
    include_lib;
kind_to_category(Kind) when Kind =:= behaviour ->
    behaviour.
