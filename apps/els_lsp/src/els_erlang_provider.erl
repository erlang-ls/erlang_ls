-module(els_erlang_provider).

-behaviour(els_provider).

-export([
    is_enabled/0,
    handle_request/1
]).

-include("els_lsp.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any()) -> {response, any()}.
handle_request({edoc, Params}) ->
    #{
      <<"textDocument">> := #{<<"uri">> := Uri}
     } = Params,
    M = els_uri:module(Uri),
    {ok, Doc} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:pois(Doc, [function]),
    %% TODO: Add module docs
    %% TODO: Only compute once
    %% TODO: Background job
    Output = els_utils:to_binary(
               lists:flatten(
                 io_lib:format("~p", [[els_docs:edoc(M, F, A) || #{id := {F, A}} <- POIs]]))),
    {response, #{name => Output}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
