-module(els_diagnostics_provider).

-behaviour(els_provider).

-export([ is_enabled/0
        , options/0
        , handle_request/2
        ]).

-export([ notify/2
        , publish/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{}.

-spec handle_request(any(), any()) -> {diagnostics, uri(), [pid()]}.
handle_request({run_diagnostics, Params}, _State) ->
  #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
  ?LOG_DEBUG("Starting diagnostics jobs [uri=~p]", [Uri]),
  Jobs = els_diagnostics:run_diagnostics(Uri),
  {diagnostics, Uri, Jobs}.

%%==============================================================================
%% API
%%==============================================================================
-spec notify([els_diagnostics:diagnostic()], pid()) -> ok.
notify(Diagnostics, Job) ->
  els_provider ! {diagnostics, Diagnostics, Job},
  ok.

-spec publish(uri(), [els_diagnostics:diagnostic()]) -> ok.
publish(Uri, Diagnostics) ->
  Method = <<"textDocument/publishDiagnostics">>,
  Params = #{ uri         => Uri
            , diagnostics => Diagnostics
            },
  els_server:send_notification(Method, Params).
