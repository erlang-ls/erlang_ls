-module(erlang_ls_references_index).

-behaviour(erlang_ls_indexer).

-compile({no_auto_import, [get/1]}).

-export([ index/1
        ]).

-export([get/2]).

-type key() :: {module(), atom()}.
-type ref() :: #{uri := uri(), range := poi_range()}.

-export_type([key/0]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_index functions
%%==============================================================================

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  Uri   = erlang_ls_document:uri(Document),
  Kinds = [application, implicit_fun],
  POIs  = erlang_ls_document:points_of_interest(Document, Kinds),
  [register_usage(Uri, POI) || POI <- POIs],
  ok.

%%==============================================================================
%% External functions
%%==============================================================================

-spec get(uri(), poi()) -> [ref()].
get(Uri, #{kind := Kind, data := MFA})
  when Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= function;
       Kind =:= exports_entry ->
  Key = key(Uri, MFA),
  ordsets:to_list(get(Key));
get(_, _) ->
  [].

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec register_usage(uri(), poi()) -> ok.
register_usage(Uri, #{data := MFA, range := Range}) ->
  Key = key(Uri, MFA),
  Ref = #{uri => Uri, range => Range},
  add(Key, Ref),
  ok.

-spec get(key()) -> ordsets:ordset(ref()).
get(Key) ->
  case erlang_ls_db:find(references_index, Key) of
    {ok, Refs}         -> Refs;
    {error, not_found} -> ordsets:new()
  end.

-spec add(key(), ref()) -> ok.
add(Key, Value) ->
  Refs = ordsets:add_element(Value, get(Key)),
  ok = erlang_ls_db:store(references_index, Key, Refs).

-spec key(uri(), {module(), atom(), arity()} | {atom(), arity()}) ->
  key().
key(_Uri, {M, F, _A}) ->
  {M, F};
key(Uri, {F, _A}) ->
  Module = erlang_ls_uri:module(Uri),
  {Module, F}.
