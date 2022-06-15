-module(els_semantic_token_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").
-export([handle_request/1]).

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec handle_request(any()) -> {response, any()}.
handle_request({semantic_tokens, Params}) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    Result = #{<<"data">> => semantic_tokens(Uri)},
    {response, Result}.

-spec semantic_tokens(uri()) -> [integer()].
semantic_tokens(Uri) ->
    wrangler_handler:get_semantic_tokens(Uri).
