-module(els_diagnostics_provider).

-behaviour(els_provider).

-export([
    options/0,
    handle_request/1
]).

-export([
    notify/2,
    publish/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec options() -> map().
options() ->
    #{}.

-spec handle_request(any()) -> {diagnostics, uri(), [pid()]}.
handle_request({run_diagnostics, Params}) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    ?LOG_DEBUG("Starting diagnostics jobs [uri=~p]", [Uri]),
    Jobs = els_diagnostics:run_diagnostics(Uri),
    {diagnostics, Uri, Jobs}.

%%==============================================================================
%% API
%%==============================================================================
-spec notify([els_diagnostics:diagnostic()], pid()) -> ok.
notify(Diagnostics, Job) ->
    els_server ! {diagnostics, Diagnostics, Job},
    ok.

-spec publish(uri(), [els_diagnostics:diagnostic()]) -> ok.
publish(Uri, Diagnostics) ->
    Method = <<"textDocument/publishDiagnostics">>,
    Params = #{
        uri => Uri,
        diagnostics => Diagnostics
    },
    els_server:send_notification(Method, Params).
