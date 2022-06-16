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
handle_request({edoc, _Params}) ->
    {response, #{name => <<"Hello from Erlang LS!">>}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
