-module(erlang_ls_completion_index).

-behaviour(erlang_ls_index).

-export([ index/1
        , setup/0
        ]).

-export([ find/1 ]).

-type key()   :: module().
-type value() :: uri().

-export_type([ key/0 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_index functions
%%==============================================================================

-spec setup() -> ok.
setup() ->
  ok.

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  Uri    = erlang_ls_document:uri(Document),
  Module = erlang_ls_uri:module(Uri),
  %% TODO: Use callback function to fetch index name
  ok = erlang_ls_db:store(completion_index, Module, Uri).

%%==============================================================================
%% External functions
%%==============================================================================

-spec find(module()) -> {ok, value()} | {error, not_found}.
find(Module) ->
  %% TODO: Use callback function to fetch index name
  erlang_ls_db:find(completion_index, Module).
