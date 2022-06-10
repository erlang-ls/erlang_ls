-module(els_provider).

%% API
-export([
    handle_request/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_core.hrl").

-callback handle_request(provider_request()) -> provider_result().

-type provider() :: atom().
-type provider_request() :: {atom() | binary(), map()}.
-type provider_result() ::
    {async, uri(), pid()}
    | {response, any()}
    | {diagnostics, uri(), [pid()]}
    | noresponse.

-export_type([
    provider/0,
    provider_request/0,
    provider_result/0
]).

%%==============================================================================
%% API
%%==============================================================================
-spec handle_request(provider(), provider_request()) -> provider_result().
handle_request(Provider, Request) ->
    Provider:handle_request(Request).
