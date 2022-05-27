-module(els_semantic_token_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").
-export([handle_request/2, is_enabled/0]).

%%==============================================================================
%% els_provider functions
%%==============================================================================

-type state() :: any().

-spec is_enabled() -> boolean().
is_enabled() ->
    wrangler_handler:is_enabled(). %% Currently this is used by Wrangler only.

-spec handle_request(els_provider:request(), state()) -> {response, any()}.
handle_request({semantic_tokens, Params}, _State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    Result = #{<<"data">> => semantic_tokens(Uri)},
    {response, Result}.

-spec semantic_tokens(uri()) -> [integer()].
semantic_tokens(Uri) ->
    wrangler_handler:get_semantic_tokens(Uri).
