%%==============================================================================
%% The 'references' table
%%==============================================================================

-module(els_dt_references).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-behaviour(els_db_table).
-export([ name/0
        , opts/0
        ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ delete_by_uri/1
        , find_by/1
        , find_by_id/2
        , insert/2
        ]).

%%==============================================================================
%% Test API
%%==============================================================================

-export([ find_candidate_uris/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_references, { id    :: any()       | '_'
                           , uri   :: uri()       | '_'
                           , range :: poi_range() | '_'
                           }).
-type els_dt_references() :: #els_dt_references{}.

-type item() :: #{ id    := any()
                 , uri   := uri()
                 , range := poi_range()
                 }.
-export_type([ item/0 ]).

-type poi_category() :: function
                      | type
                      | macro
                      | record
                      | include
                      | include_lib
                      | behaviour.

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

-spec from_item(poi_kind(), item()) -> els_dt_references().
from_item(Kind, #{ id := Id, uri := Uri, range := Range}) ->
  InternalId = {kind_to_category(Kind), Id},
  #els_dt_references{ id = InternalId, uri = Uri, range = Range}.

-spec to_item(els_dt_references()) -> item().
to_item(#els_dt_references{ id = {_Category, Id}, uri = Uri, range = Range }) ->
  #{ id    => Id
   , uri   => Uri
   , range => Range
   }.

-spec delete_by_uri(uri()) -> ok | {error, any()}.
delete_by_uri(Uri) ->
  Pattern = #els_dt_references{uri = Uri, _ = '_'},
  ok = els_db:match_delete(name(), Pattern).

-spec insert(poi_kind(), item()) -> ok | {error, any()}.
insert(Kind, Map) when is_map(Map) ->
  Record = from_item(Kind, Map),
  els_db:write(name(), Record).

%% @doc Find by id
-spec find_by_id(poi_kind(), any()) -> {ok, [item()]} | {error, any()}.
find_by_id(Kind, Id) ->
  InternalId = {kind_to_category(Kind), Id},
  Pattern = #els_dt_references{id = InternalId, _ = '_'},
  find_by(Pattern).

-spec find_by(tuple()) -> {ok, [item()]}.
find_by(#els_dt_references{id = Id} = Pattern) ->
  Uris = find_candidate_uris(Id),
  [begin
     {ok, Document} = els_utils:lookup_document(Uri),
     index_references(Document)
   end || Uri <- Uris],
  {ok, Items} = els_db:match(name(), Pattern),
  {ok, [to_item(Item) || Item <- Items]}.

-spec index_references(els_dt_document:id()) -> ok.
index_references(#{uri := Uri, pois := ondemand} = Document) ->
  POIs = els_dt_document:pois(Document, [ application
                                        , behaviour
                                        , implicit_fun
                                        , include
                                        , include_lib
                                        , type_application
                                        , import_entry
                                        ]),
  [register_reference(Uri, POI) || POI <- POIs],
  ok;
index_references(_) ->
  ok.

%% TODO: What about references from header files?
-spec greppable_string({poi_category(), any()}) ->
        {ok, string()} | {error, {any(), not_supported}}.
greppable_string({function, {_M, F, _A}}) ->
  io_lib:format("~p", [F]);
greppable_string({type, {_M, F, _A}}) ->
  io_lib:format("~p", [F]);
greppable_string({macro, {Name, _Arity}}) ->
  io_lib:format("~p", [Name]);
greppable_string({macro, Name}) ->
  io_lib:format("~p", [Name]);
greppable_string({include, String}) ->
  io_lib:format("~s", [String]);
%% TODO: This could be tricky
greppable_string({include_lib, String}) ->
  io_lib:format("~s", [String]);
greppable_string({behaviour, Name}) ->
  io_lib:format("~p", [Name]).

-spec find_candidate_uris(any()) -> [uri()].
find_candidate_uris(Id) ->
  IdString = greppable_string(Id),
  Paths = els_config:get(apps_paths) ++ els_config:get(deps_paths),
  PathsString = string:join(Paths, " "),
  Cmd = "grep -l -r \"" ++ IdString ++ "\" " ++ PathsString,
  ?LOG_DEBUG("Command: ~p", [Cmd]),
  Result = string:trim(os:cmd(Cmd), trailing),
  ?LOG_DEBUG("Result: ~p", [Result]),
  Candidates = string:split(Result, "\n", all),
  [els_uri:uri(els_utils:to_binary(Candidate)) || Candidate <- Candidates,
                                                  Candidate =/= []].

-spec kind_to_category(poi_kind()) -> poi_category().
kind_to_category(Kind) when Kind =:= application;
                            Kind =:= export_entry;
                            Kind =:= function;
                            Kind =:= function_clause;
                            Kind =:= import_entry;
                            Kind =:= implicit_fun ->
  function;
kind_to_category(Kind) when Kind =:= export_type_entry;
                            Kind =:= type_application;
                            Kind =:= type_definition ->
  type;
kind_to_category(Kind) when Kind =:= macro;
                            Kind =:= define ->
  macro;
kind_to_category(Kind) when Kind =:= record_expr;
                            Kind =:= record ->
  record;
kind_to_category(Kind) when Kind =:= include ->
  include;
kind_to_category(Kind) when Kind =:= include_lib ->
  include_lib;
kind_to_category(Kind) when Kind =:= behaviour ->
  behaviour.

-spec register_reference(uri(), poi()) -> ok.
register_reference(Uri, #{id := {F, A}} = POI) ->
  M = els_uri:module(Uri),
  register_reference(Uri, POI#{id => {M, F, A}});
register_reference(Uri, #{kind := Kind, id := Id, range := Range})
  when %% Include
       Kind =:= include;
       Kind =:= include_lib;
       %% Function
       Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= import_entry;
       %% Type
       Kind =:= type_application;
       %% Behaviour
       Kind =:= behaviour ->
  insert( Kind
        , #{id => Id, uri => Uri, range => Range}
        ).
