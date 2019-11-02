-module(erlang_ls_methods).

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
%% @doc Dispatch the handling of the method to erlang_ls_method
%%==============================================================================
-spec dispatch(method_name(), params(), state()) -> result().
dispatch(Method, Params, State) ->
  Function = method_to_function_name(Method),
  try do_dispatch(Function, Params, State)
  catch error:undef -> not_implemented_method(Method, State)
  end.

-spec do_dispatch(atom(), params(), state()) -> result().
do_dispatch(exit, Params, State) ->
  erlang_ls_methods:exit(Params, State);
do_dispatch(_Function, _Params, #{status := shutdown} = State) ->
  Message = <<"Server is shutting down">>,
  Result  = #{ code    => ?ERR_INVALID_REQUEST
             , message => Message
             },
  {error, Result, State};
do_dispatch(Function, Params, State) ->
  erlang_ls_methods:Function(Params, State).

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
     %% TODO: Store client capabilities, use them in completion_provider
     %%       to verify when Context is present
   , <<"capabilities">> := _ClientCapabilities
   } = Params,
  InitOptions = maps:get(<<"initializationOptions">>, Params, #{}),
  ok = erlang_ls_config:initialize(RootUri, InitOptions),
  [erlang_ls_indexer:index_dir(Dir) || Dir <- erlang_ls_config:get(app_paths)],
  ok = erlang_ls_provider:initialize(),
  Result =
    #{ capabilities =>
         #{ hoverProvider => erlang_ls_hover_provider:is_enabled()
          , completionProvider =>
              #{ resolveProvider => false
               , triggerCharacters => [<<":">>, <<"#">>, <<"?">>]
               }
          , textDocumentSync =>
              #{ openClose => true
               , change    => ?TEXT_DOCUMENT_SYNC_KIND_FULL
               , save      => true
               }
          , definitionProvider => erlang_ls_definition_provider:is_enabled()
          , referencesProvider => erlang_ls_references_provider:is_enabled()
          , documentSymbolProvider =>
              erlang_ls_document_symbol_provider:is_enabled()
          , workspaceSymbolProvider =>
              erlang_ls_workspace_symbol_provider:is_enabled()
          }
     },
  {response, Result, State#{status => initialized}}.

%%==============================================================================
%% Initialized
%%==============================================================================

-spec initialized(params(), state()) -> result().
initialized(_Params, State) ->
  {noresponse, State#{status => initialized}}.

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
  erlang_ls_utils:halt(ExitCode).

%%==============================================================================
%% textDocument/didopen
%%==============================================================================

-spec textdocument_didopen(params(), state()) -> result().
textdocument_didopen(Params, State) ->
  ok = erlang_ls_text_synchronization:did_open(Params),
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
      Document = erlang_ls_document:create(Uri, Text),
      erlang_ls_indexer:index(Document)
  end,
  {noresponse, State}.

%%==============================================================================
%% textDocument/didsave
%%==============================================================================

-spec textdocument_didsave(params(), state()) -> result().
textdocument_didsave(Params, State) ->
  spawn(erlang_ls_text_synchronization, did_save, [Params, self()]),
  {noresponse, State}.

%%==============================================================================
%% textDocument/didclose
%%==============================================================================

-spec textdocument_didclose(params(), state()) -> result().
textdocument_didclose(Params, State) ->
  ok = erlang_ls_text_synchronization:did_close(Params),
  {noresponse, State}.

%%==============================================================================
%% textdocument/documentSymbol
%%==============================================================================

-spec textdocument_documentsymbol(params(), state()) -> result().
textdocument_documentsymbol(Params, State) ->
  Provider = erlang_ls_document_symbol_provider,
  Request  = {document_symbol, Params},
  Response = erlang_ls_provider:handle_request(Provider, Request),
  {response, Response, State}.

%%==============================================================================
%% textDocument/hover
%%==============================================================================

-spec textdocument_hover(params(), state()) -> result().
textdocument_hover(Params, State) ->
  Provider = erlang_ls_hover_provider,
  Response = erlang_ls_provider:handle_request(Provider, {hover, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/completion
%%==============================================================================

-spec textdocument_completion(params(), state()) -> result().
textdocument_completion(Params, State) ->
  Provider = erlang_ls_completion_provider,
  Response = erlang_ls_provider:handle_request(Provider, {completion, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/definition
%%==============================================================================

-spec textdocument_definition(params(), state()) -> result().
textdocument_definition(Params, State) ->
  Provider = erlang_ls_definition_provider,
  Response = erlang_ls_provider:handle_request(Provider, {definition, Params}),
  {response, Response, State}.

%%==============================================================================
%% textDocument/references
%%==============================================================================

-spec textdocument_references(params(), state()) -> result().
textdocument_references(Params, State) ->
  Provider = erlang_ls_references_provider,
  Response = erlang_ls_provider:handle_request(Provider, {references, Params}),
  {response, Response, State}.

%%==============================================================================
%% workspace/symbol
%%==============================================================================

-spec workspace_symbol(map(), state()) -> result().
workspace_symbol(Params, State) ->
  Provider = erlang_ls_workspace_symbol_provider,
  Response = erlang_ls_provider:handle_request(Provider, {symbol, Params}),
  {response, Response, State}.
