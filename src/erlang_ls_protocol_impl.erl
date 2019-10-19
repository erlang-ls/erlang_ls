-module(erlang_ls_protocol_impl).

-export([ initialize/1
        , initialized/1
        , shutdown/1
        , exit/1
        ]).

-export([ textdocument_completion/1
        , textdocument_didopen/1
        , textdocument_didchange/1
        , textdocument_didsave/1
        , textdocument_didclose/1
        , textdocument_hover/1
        , textdocument_definition/1
        , textdocument_references/1
        ]).

%%==============================================================================
%% Macros
%%==============================================================================
-define(DEFAULT_CONFIG_PATH, "erlang_ls.config").

%%==============================================================================
%% Initialize
%%==============================================================================

-spec initialize(map()) ->
  {response, map()} | {} | {notification, binary(), map()}.
initialize(Params) ->
  #{ <<"rootUri">> := RootUri
   , <<"initializationOptions">> := InitOptions
     %% TODO: Store client capabilities, use them in completion_provider
     %%       to verify when Context is present
   , <<"capabilities">> := _ClientCapabilities
   } = Params,
  Config = consult_config(filename:join([ erlang_ls_uri:path(RootUri)
                                        , config_path(InitOptions)
                                        ])),
  OtpPath = maps:get("otp_path", Config, code:root_dir()),
  DepsDirs = maps:get("deps_dirs", Config, []),
  ok = erlang_ls_config:set(root_uri, RootUri),
  ok = erlang_ls_config:set(otp_path, OtpPath),
  ok = erlang_ls_config:set(deps_dirs, DepsDirs),
  ok = erlang_ls_index:initialize(Config),
  ok = erlang_ls_provider:initialize(Config),
  Result =
    #{ capabilities =>
         #{ hoverProvider => false
          , completionProvider =>
              #{ resolveProvider => false
               , triggerCharacters => [<<":">>, <<"#">>]
               }
          , textDocumentSync => 1
          , definitionProvider => erlang_ls_definition_provider:is_enabled()
          , referencesProvider => erlang_ls_references_provider:is_enabled()
          }
     },
  {response, Result}.

-spec config_path(map()) -> erlang_ls_uri:path().
config_path(#{<<"erlang">> := #{<<"config_path">> := ConfigPath}}) ->
  ConfigPath;
config_path(_) ->
  ?DEFAULT_CONFIG_PATH.

-spec consult_config(erlang_ls_uri:path()) -> map().
consult_config(Path) ->
  lager:info("Reading config file. path=~p", [Path]),
  Options = [{map_node_format, map}],
  try yamerl:decode_file(Path, Options) of
      [] -> #{};
      [Config] -> Config
  catch
    Class:Error ->
      lager:warning( "Error reading config file. path=~p class=~p error=~p"
                   , [Path, Class, Error]),
      #{}
  end.

%%==============================================================================
%% Initialized
%%==============================================================================

-spec initialized(map()) ->
  {response, map()} | {} | {notification, binary(), map()}.
initialized(_Params) ->
  {}.

%%==============================================================================
%% shutdown
%%==============================================================================

-spec shutdown(map()) ->
  {response, null}.
shutdown(_Params) ->
  %% TODO: keep in the state that we got a shutdown
  {response, null}.

%%==============================================================================
%% exit
%%==============================================================================

-spec exit(map()) ->
  {response, map()} | {} | {notification, binary(), map()}.
exit(_Params) ->
  %% TODO: exit with 1 if shutdown wasnt sent before
  erlang:halt(0).

%%==============================================================================
%% textdocument_didopen
%%==============================================================================

-spec textdocument_didopen(map()) ->
  {response, map()} | {} | {notification, binary(), map()}.
textdocument_didopen(Params) ->
  ok = erlang_ls_text_synchronization:did_open(Params),
  {}.

%%==============================================================================
%% textdocument_didchange
%%==============================================================================

-spec textdocument_didchange(map()) ->
  {response, map()} | {} | {notification, binary(), map()}.
textdocument_didchange(Params) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  TextDocument   = maps:get(<<"textDocument">>  , Params),
  Uri            = maps:get(<<"uri">>           , TextDocument),
  case ContentChanges of
    []                      -> ok;
    [#{<<"text">> := Text}] ->
      Document = erlang_ls_document:create(Uri, Text),
      erlang_ls_index:index(Document)
  end,
  {}.

%%==============================================================================
%% textdocument_didsave
%%==============================================================================

-spec textdocument_didsave(map()) -> {}.
textdocument_didsave(Params) ->
  spawn(erlang_ls_text_synchronization, did_save, [Params, self()]),
  {}.

%%==============================================================================
%% textdocument_didclose
%%==============================================================================

-spec textdocument_didclose(map()) -> {}.
textdocument_didclose(Params) ->
  ok = erlang_ls_text_synchronization:did_close(Params),
  {}.

%%==============================================================================
%% textdocument_hover
%%==============================================================================

-spec textdocument_hover(map()) -> {response, null}.
textdocument_hover(_Params) ->
  {response, null}.

%%==============================================================================
%% textdocument_completion
%%==============================================================================

-spec textdocument_completion(map()) -> {response, map()}.
textdocument_completion(Params) ->
  Provider = erlang_ls_completion_provider,
  Response = erlang_ls_provider:handle_request(Provider, {completion, Params}),
  {response, Response}.

%%==============================================================================
%% textdocument_definition
%%==============================================================================

-spec textdocument_definition(map()) -> {response, map() | null}.
textdocument_definition(Params) ->
  Provider = erlang_ls_definition_provider,
  Response = erlang_ls_provider:handle_request(Provider, {definition, Params}),
  {response, Response}.

%%==============================================================================
%% textdocument_references
%%==============================================================================

-spec textdocument_references(map()) -> {response, map() | null}.
textdocument_references(Params) ->
  Provider = erlang_ls_references_provider,
  Response = erlang_ls_provider:handle_request(Provider, {references, Params}),
  {response, Response}.
