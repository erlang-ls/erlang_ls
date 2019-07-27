-module(erlang_ls_text_synchronization).

-export([ did_open/1
        , did_save/2
        , did_close/1
        ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  {ok, Pid}    = supervisor:start_child(erlang_ls_buffer_sup, [Text]),
  ok           = erlang_ls_buffer_server:add_buffer(Uri, Pid).

-spec did_save(map(), pid()) -> ok.
did_save(Params, Server) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  CDiagnostics = erlang_ls_compiler_diagnostics:diagnostics(Uri),
  DDiagnostics = erlang_ls_dialyzer_diagnostics:diagnostics(Uri),
  Method = <<"textDocument/publishDiagnostics">>,
  Params1  = #{ uri => Uri
              , diagnostics => CDiagnostics ++ DDiagnostics
              },
  %% TODO: prune/re-index on save/change
  gen_server:cast(Server, {notification, Method, Params1}).

-spec did_close(map()) -> ok.
did_close(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  case Buffer of
    undefined ->
      lager:debug("[SERVER] Attempting to close un-opened text document, ignoring [uri=~p]", [Uri]);
    B when is_pid(B) ->
      ok = erlang_ls_buffer_server:remove_buffer(Uri),
      ok = supervisor:terminate_child(erlang_ls_buffer_sup, Buffer)
  end,
  ok.
