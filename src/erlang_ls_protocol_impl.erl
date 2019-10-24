-module(erlang_ls_protocol_impl).

-include("erlang_ls.hrl").

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

-type state()   :: map().
-type params()  :: map().
-type result()  :: {response, params() | null, state()}
                 | {noresponse, state()}
                 | {notification, binary(), params(), state()}.

%%==============================================================================
%% Initialize
%%==============================================================================

-spec initialize(params(), state()) -> result().
initialize(Params, State) ->
  #{ <<"rootUri">> := RootUri
   , <<"initializationOptions">> := InitOptions
     %% TODO: Store client capabilities, use them in completion_provider
     %%       to verify when Context is present
   , <<"capabilities">> := _ClientCapabilities
   } = Params,
  Config = erlang_ls_config:initialize(RootUri, InitOptions),
  ok     = erlang_ls_index:initialize(Config),
  ok     = erlang_ls_provider:initialize(Config),
  Result =
    #{ capabilities =>
         #{ hoverProvider => false
          , completionProvider =>
              #{ resolveProvider => false
               , triggerCharacters => [<<":">>, <<"#">>]
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

-spec exit(params(), state()) -> result().
exit(_Params, State) ->
  ExitCode = case maps:get(status, State, undefined) of
               shutdown -> 0;
               _        -> 1
             end,
  erlang:halt(ExitCode).

%%==============================================================================
%% textdocument_didopen
%%==============================================================================

-spec textdocument_didopen(params(), state()) -> result().
textdocument_didopen(Params, State) ->
  ok = erlang_ls_text_synchronization:did_open(Params),
  {noresponse, State}.

%%==============================================================================
%% textdocument_didchange
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
      erlang_ls_index:index(Document)
  end,
  {noresponse, State}.

%%==============================================================================
%% textdocument_didsave
%%==============================================================================

-spec textdocument_didsave(params(), state()) -> result().
textdocument_didsave(Params, State) ->
  spawn(erlang_ls_text_synchronization, did_save, [Params, self()]),
  {noresponse, State}.

%%==============================================================================
%% textdocument_didclose
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
%% textdocument_hover
%%==============================================================================

-spec textdocument_hover(params(), state()) -> result().
textdocument_hover(_Params, State) ->
  {response, null, State}.

%%==============================================================================
%% textdocument_completion
%%==============================================================================

-spec textdocument_completion(params(), state()) -> result().
textdocument_completion(Params, State) ->
  Provider = erlang_ls_completion_provider,
  Response = erlang_ls_provider:handle_request(Provider, {completion, Params}),
  {response, Response, State}.

%%==============================================================================
%% textdocument_definition
%%==============================================================================

-spec textdocument_definition(params(), state()) -> result().
textdocument_definition(Params, State) ->
  Provider = erlang_ls_definition_provider,
  Response = erlang_ls_provider:handle_request(Provider, {definition, Params}),
  {response, Response, State}.

%%==============================================================================
%% textdocument_references
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
