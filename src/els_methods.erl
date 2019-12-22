-module(els_methods).

-include("erlang_ls.hrl").

-export([ dispatch/3
        ]).

-export([ initialize/2
        , initialized/2
        , shutdown/2
        , exit/2
        ]).

-export([ textdocument_completion/2
        , textdocument_didopen/2
        , textdocument_didchange/2
        , textdocument_didsave/2
        , textdocument_didclose/2
        , textdocument_documentsymbol/2
        , textdocument_hover/2
        , textdocument_definition/2
        , textdocument_references/2
        , textdocument_documenthighlight/2
        , textdocument_formatting/2
        , textdocument_rangeformatting/2
        , textdocument_ontypeformatting/2
        , workspace_didchangewatchedfiles/2
        , workspace_symbol/2
        ]).

-type method_name() :: binary().
-type state()       :: map().
-type params()      :: map().
-type result()      :: {response, params() | null, state()}
                     | {error, params(), state()}
                     | {noresponse, state()}
                     | {notification, binary(), params(), state()}.

%%==============================================================================
%% @doc Dispatch the handling of the method to els_method
%%==============================================================================
-spec dispatch(method_name(), params(), state()) -> result().
dispatch(Method, Params, State) ->
  Function = method_to_function_name(Method),
  try do_dispatch(Function, Params, State)
  catch
    error:undef ->
      not_implemented_method(Method, State);
    Type:Reason:Stack ->
      lager:error( "Unexpected error [type=~p] [error=~p] [stack=~p]"
                 , [Type, Reason, Stack]),
      Error = #{ code    => ?ERR_UNKNOWN_ERROR_CODE
               , message => <<"Unexpected error while ", Method/binary>>
               },
      {error, Error, State}
  end.

-spec do_dispatch(atom(), params(), state()) -> result().
do_dispatch(exit, Params, State) ->
  els_methods:exit(Params, State);
do_dispatch(_Function, _Params, #{status := shutdown} = State) ->
  Message = <<"Server is shutting down">>,
  Result  = #{ code    => ?ERR_INVALID_REQUEST
             , message => Message
             },
  {error, Result, State};
do_dispatch(initialize, Params, State) ->
  els_methods:initialize(Params, State);
do_dispatch(Function, Params, #{status := initialized} = State) ->
  els_methods:Function(Params, State);
do_dispatch(_Function, _Params, State) ->
  Message = <<"The server is not fully initialized yet, please wait.">>,
  Result  = #{ code    => ?ERR_SERVER_NOT_INITIALIZED
             , message => Message
             },
  {error, Result, State}.

-spec not_implemented_method(method_name(), state()) -> result().
not_implemented_method(Method, State) ->
  lager:warning("[Method not implemented] [method=~s]", [Method]),
  Message = <<"Method not implemented: ", Method/binary>>,
  Method1 = <<"window/showMessage">>,
  Params  = #{ type    => ?MESSAGE_TYPE_INFO
             , message => Message
             },
  {notification, Method1, Params, State}.

-spec method_to_function_name(method_name()) -> atom().
method_to_function_name(Method) ->
  Replaced = string:replace(Method, <<"/">>, <<"_">>),
  Lower    = string:lowercase(Replaced),
  Binary   = erlang:iolist_to_binary(Lower),
  binary_to_atom(Binary, utf8).

%%==============================================================================
%% Initialize
%%==============================================================================

-spec initialize(params(), state()) -> result().
initialize(Params, State) ->
  #{ <<"rootUri">> := RootUri
     %% TODO: Use ClientCapabilities in completion_provider
     %%       to verify when Context is present
   , <<"capabilities">> := Capabilities
   } = Params,
  InitOptions = maps:get(<<"initializationOptions">>, Params, #{}),
  ok = els_config:initialize(RootUri, Capabilities, InitOptions),
  DbDir = application:get_env(erlang_ls, db_dir, default_db_dir()),
  els_db:install( node_name(RootUri, list_to_binary(els_config:get(otp_path)))
                , DbDir
                ),
  els_indexer:index_apps(),
  els_indexer:index_deps(),
  els_indexer:index_otp(),
  ok = els_provider:initialize(),
  Result =
    #{ capabilities =>
         #{ textDocumentSync =>
              #{ openClose => true
               , change    => ?TEXT_DOCUMENT_SYNC_KIND_FULL
               , save      => #{includeText => true}
               }
          , hoverProvider => els_hover_provider:is_enabled()
          , completionProvider =>
              #{ resolveProvider => false
               , triggerCharacters => [<<":">>, <<"#">>, <<"?">>, <<".">>]
               }
          , definitionProvider => els_definition_provider:is_enabled()
          , referencesProvider => els_references_provider:is_enabled()
          , documentHighlightProvider =>
              els_document_highlight_provider:is_enabled()
          , documentSymbolProvider =>
              els_document_symbol_provider:is_enabled()
          , workspaceSymbolProvider =>
              els_workspace_symbol_provider:is_enabled()
          , documentFormattingProvider =>
              els_formatting_provider:is_enabled_document()
          , documentRangeFormattingProvider =>
              els_formatting_provider:is_enabled_range()
          , documentOnTypeFormattingProvider =>
              els_formatting_provider:is_enabled_on_type()
          %% AZ: didchangewatchedfiles is not listed in
          %%     ServerCapabilities in the LSP spec.
          , didChangeWatchedFiles =>
              #{ dynamicRegistration => false }
          }
     },
  {response, Result, State#{status => initialized}}.

%%==============================================================================
%% Initialized
%%==============================================================================

-spec initialized(params(), state()) -> result().
initialized(_Params, State) ->
  %% Report to the user the server version
  {ok,     App} = application:get_application(),
  {ok, Version} = application:get_key(App, vsn),
  lager:info("initialized: [App=~p] [Version=~p]", [App, Version]),
  BinVersion = list_to_binary(Version),
  Message = <<"Erlang LS version: ", BinVersion/binary>>,
  Method  = <<"window/showMessage">>,
  Params  = #{ type    => ?MESSAGE_TYPE_INFO
             , message => Message
             },
  {notification, Method, Params, State#{status => initialized}}.

%%==============================================================================
%% shutdown
%%==============================================================================

-spec shutdown(params(), state()) -> result().
shutdown(_Params, State) ->
  {response, null, State#{status => shutdown}}.

%%==============================================================================
%% exit
%%==============================================================================

-spec exit(params(), state()) -> no_return().
exit(_Params, State) ->
  ExitCode = case maps:get(status, State, undefined) of
               shutdown -> 0;
               _        -> 1
             end,
  els_utils:halt(ExitCode).

%%==============================================================================
%% textDocument/didopen
%%==============================================================================

-spec textdocument_didopen(params(), state()) -> result().
textdocument_didopen(Params, State) ->
  ok = els_text_synchronization:did_open(Params),
  {noresponse, State}.

%%==============================================================================
%% textDocument/didchange
%%==============================================================================

-spec textdocument_didchange(params(), state()) -> result().
textdocument_didchange(Params, State) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  TextDocument   = maps:get(<<"textDocument">>  , Params),
  Uri            = maps:get(<<"uri">>           , TextDocument),
  case ContentChanges of
    []                      -> ok;
    [#{<<"text">> := Text}] ->
      Document = els_dt_document:new(Uri, Text),
      els_indexer:index(Document)
  end,
  {noresponse, State}.

%%==============================================================================
%% textDocument/didsave
%%==============================================================================

-spec textdocument_didsave(params(), state()) -> result().
textdocument_didsave(Params, State) ->
  ok = els_text_synchronization:did_save(Params),
  {noresponse, State}.

%%==============================================================================
%% textDocument/didclose
%%==============================================================================

-spec textdocument_didclose(params(), state()) -> result().
textdocument_didclose(Params, State) ->
  ok = els_text_synchronization:did_close(Params),
  {noresponse, State}.

%%==============================================================================
%% textdocument/documentSymbol
%%==============================================================================

-spec textdocument_documentsymbol(params(), state()) -> result().
textdocument_documentsymbol(Params, State) ->
  Provider = els_document_symbol_provider,
  Request  = {document_symbol, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State}.

%%==============================================================================
%% textDocument/hover
%%==============================================================================

-spec textdocument_hover(params(), state()) -> result().
textdocument_hover(Params, State) ->
  Provider = els_hover_provider,
  Response = els_provider:handle_request(Provider, {hover, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/completion
%%==============================================================================

-spec textdocument_completion(params(), state()) -> result().
textdocument_completion(Params, State) ->
  Provider = els_completion_provider,
  Response = els_provider:handle_request(Provider, {completion, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/definition
%%==============================================================================

-spec textdocument_definition(params(), state()) -> result().
textdocument_definition(Params, State) ->
  Provider = els_definition_provider,
  Response = els_provider:handle_request(Provider, {definition, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/references
%%==============================================================================

-spec textdocument_references(params(), state()) -> result().
textdocument_references(Params, State) ->
  Provider = els_references_provider,
  Response = els_provider:handle_request(Provider, {references, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/documentHightlight
%%==============================================================================

-spec textdocument_documenthighlight(params(), state()) -> result().
textdocument_documenthighlight(Params, State) ->
  Provider = els_document_highlight_provider,
  Response = els_provider:handle_request(Provider,
                                         {document_highlight, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/formatting
%%==============================================================================

-spec textdocument_formatting(params(), state()) -> result().
textdocument_formatting(Params, State) ->
  Provider = els_formatting_provider,
  Response = els_provider:handle_request(Provider,
                                         {document_formatting, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/rangeFormatting
%%==============================================================================

-spec textdocument_rangeformatting(params(), state()) -> result().
textdocument_rangeformatting(Params, State) ->
  Provider = els_formatting_provider,
  Response = els_provider:handle_request(Provider,
                                         {document_rangeformatting, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/onTypeFormatting
%%==============================================================================

-spec textdocument_ontypeformatting(params(), state()) -> result().
textdocument_ontypeformatting(Params, State) ->
  Provider = els_formatting_provider,
  Response = els_provider:handle_request(Provider,
                                         {document_ontypeformatting, Params}),
  {response, Response, State}.

%%==============================================================================
%% workspace/didChangeWatchedFiles
%%==============================================================================

-spec workspace_didchangewatchedfiles(map(), state()) -> result().
workspace_didchangewatchedfiles(_Params, State) ->
  %% Some clients rely on these notifications to be successful.
  %% Let's just ignore them.
  {noresponse, State}.

%%==============================================================================
%% workspace/symbol
%%==============================================================================

-spec workspace_symbol(map(), state()) -> result().
workspace_symbol(Params, State) ->
  Provider = els_workspace_symbol_provider,
  Response = els_provider:handle_request(Provider, {symbol, Params}),
  {response, Response, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec node_name(uri(), binary()) -> atom().
node_name(RootUri, OtpPath) ->
  <<SHA:160/integer>> = crypto:hash(sha, <<RootUri/binary, OtpPath/binary>>),
  list_to_atom(lists:flatten(io_lib:format("erlang_ls_~40.16.0b", [SHA]))).

-spec default_db_dir() -> string().
default_db_dir() ->
  filename:basedir(user_cache, "erlang_ls").
