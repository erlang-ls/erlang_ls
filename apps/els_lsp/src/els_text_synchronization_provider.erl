-module(els_text_synchronization_provider).

-behaviour(els_provider).
-export([ handle_request/2
        , options/0
        ]).

-include("els_lsp.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec options() -> map().
options() ->
  #{ openClose => true
   , change    => els_text_synchronization:sync_mode()
   , save      => #{includeText => false}
   }.

-spec handle_request(any(), any()) ->
        {diagnostics, uri(), [pid()]} |
        noresponse |
        {async, uri(), pid()}.
handle_request({did_open, Params}, _State) ->
  ok = els_text_synchronization:did_open(Params),
  #{<<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  {diagnostics, Uri, els_diagnostics:run_diagnostics(Uri)};
handle_request({did_change, Params}, _State) ->
  #{<<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  case els_text_synchronization:did_change(Params) of
    ok ->
      noresponse;
    {ok, Job} ->
      {async, Uri, Job}
  end;
handle_request({did_save, Params}, _State) ->
  ok = els_text_synchronization:did_save(Params),
  #{<<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  {diagnostics, Uri, els_diagnostics:run_diagnostics(Uri)};
handle_request({did_close, Params}, _State) ->
  ok = els_text_synchronization:did_close(Params),
  noresponse;
handle_request({did_change_watched_files, Params}, _State) ->
  ok = els_text_synchronization:did_change_watched_files(Params),
  noresponse.
