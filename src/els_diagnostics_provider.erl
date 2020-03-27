-module(els_diagnostics_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{}.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({run_diagnostics, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  els_diagnostics:run_diagnostics(Uri),
  {Lenses, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
