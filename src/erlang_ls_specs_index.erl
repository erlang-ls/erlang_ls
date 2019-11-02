-module(erlang_ls_specs_index).

-behaviour(erlang_ls_index).

-export([ index/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_index functions
%%==============================================================================

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  Specs  = erlang_ls_document:points_of_interest(Document, [spec]),
  Uri    = erlang_ls_document:uri(Document),
  Module = erlang_ls_uri:module(Uri),
  [erlang_ls_db:store(specs_index, {Module, F, A}, Tree) ||
    #{data := {{F, A}, Tree}} <- Specs],
  ok.
