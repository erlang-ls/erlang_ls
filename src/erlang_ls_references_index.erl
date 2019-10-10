-module(erlang_ls_references_index).

-behaviour(erlang_ls_index).

-compile({no_auto_import, [get/1]}).

-export([ index/2
        , setup/0
        ]).

-export([get/2]).

-type key() :: {module(), atom()}.
-type ref() :: #{uri := erlang_ls_uri:uri(), range := erlang_ls_poi:range()}.

-export_type([key/0]).

%%==============================================================================
%% erlang_ls_index functions
%%==============================================================================

-spec setup() -> ok.
setup() ->
  ok.

-spec index(erlang_ls_uri:uri(), any()) -> ok.
index(Uri, AnnotatedTree) ->
  [register_usage(Uri, POI) || POI <- erlang_ls_poi:list(AnnotatedTree)],
  ok.

%%==============================================================================
%% External functions
%%==============================================================================

-spec get(erlang_ls_uri:uri(), erlang_ls_poi:poi()) -> [ref()].
get(Uri, #{info := {Type, MFA}})
  when Type =:= application; Type =:= implicit_fun; Type =:= function ->
  Key = key(Uri, MFA),
  get(Key);
get(_, _) ->
  [].

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec register_usage(erlang_ls_uri:uri(), erlang_ls_poi:poi()) -> ok.
register_usage(Uri, #{info := {Type, MFA}, range := Range})
  when Type =:= application; Type =:= implicit_fun ->
  Key = key(Uri, MFA),
  Ref = #{uri => Uri, range => Range},
  add(Key, Ref),
  ok;
register_usage(_File, _POI) ->
  ok.

-spec get(key()) -> [ref()].
get(Key) ->
  case erlang_ls_db:find(references_index, Key) of
    not_found  -> [];
    {ok, Refs} -> Refs
  end.

-spec add(key(), ref()) -> ok.
add(Key, Value) ->
  Refs = get(Key),
  ok = erlang_ls_db:store(references_index, Key, [Value | Refs]).

-spec key(erlang_ls_uri:uri(), {module(), atom(), arity()} | {atom(), arity()}) ->
  key().
key(_Uri, {M, F, _A}) ->
  {M, F};
key(Uri, {F, _A}) ->
  Module = erlang_ls_uri:module(Uri),
  {Module, F}.
