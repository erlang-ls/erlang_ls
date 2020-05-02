-module(els_general_provider).

-behaviour(els_provider).
-export([ handle_request/2
        , is_enabled/0
        ]).

-export([ server_capabilities/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% Types
%%==============================================================================

-type server_capabilities() :: map().
-type initialize_request() :: {initialize, initialize_params()}.
-type initialize_params() :: #{ processId             := number() | null
                              , rootPath              => binary() | null
                              , rootUri               := uri() | null
                              , initializationOptions => any()
                              , capabilities          := client_capabilities()
                              , trace                 => off
                                                       | messages
                                                       | verbose
                              , workspaceFolders      => [workspace_folder()]
                                                       | null
                              }.
-type initialize_result() :: #{ capabilities => server_capabilities() }.
-type initialized_request() :: {initialized, initialized_params()}.
-type initialized_params() :: #{}.
-type initialized_result() :: null.
-type shutdown_request() :: {shutdown, shutdown_params()}.
-type shutdown_params() :: #{}.
-type shutdown_result() :: null.
-type exit_request() :: {exit, exit_params()}.
-type exit_params() :: #{status => atom()}.
-type exit_result() :: null.
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request( initialize_request()
                    | initialized_request()
                    | shutdown_request()
                    | exit_request()
                    , state()) ->
        { initialize_result()
        | initialized_result()
        | shutdown_result()
        | exit_result()
        , state()
        }.
handle_request({initialize, Params}, State) ->
  #{ <<"rootUri">> := RootUri0
   , <<"capabilities">> := Capabilities
   } = Params,
  RootUri = case RootUri0 of
              null ->
                {ok, Cwd} = file:get_cwd(),
                els_uri:uri(els_utils:to_binary(Cwd));
              _ -> RootUri0
            end,
  InitOptions = case maps:get(<<"initializationOptions">>, Params, #{}) of
                  InitOptions0 when is_map(InitOptions0) ->
                    InitOptions0;
                  _ -> #{}
                end,
  ok = els_config:initialize(RootUri, Capabilities, InitOptions),
  NewState = State#{ root_uri => RootUri, init_options => InitOptions},
  {server_capabilities(), NewState};
handle_request({initialized, _Params}, State) ->
  #{root_uri := RootUri, init_options := InitOptions} = State,
  DbDir = application:get_env(erlang_ls, db_dir, default_db_dir()),
  OtpPath = els_config:get(otp_path),
  NodeName = node_name(RootUri, els_utils:to_binary(OtpPath)),
  els_db:install(NodeName, DbDir),
  els_build_server:connect(),
  case maps:get(<<"indexingEnabled">>, InitOptions, true) of
    true  -> els_indexing:start();
    false -> lager:info("Skipping Indexing (disabled via InitOptions)")
  end,
  {null, State};
handle_request({shutdown, _Params}, State) ->
  {null, State};
handle_request({exit, #{status := Status}}, State) ->
  lager:info("Language server stopping..."),
  ExitCode = case Status of
               shutdown -> 0;
               _        -> 1
             end,
  els_utils:halt(ExitCode),
  {null, State}.

%%==============================================================================
%% API
%%==============================================================================

-spec server_capabilities() -> server_capabilities().
server_capabilities() ->
  #{ capabilities =>
       #{ textDocumentSync =>
            #{ openClose => true
             , change    => ?TEXT_DOCUMENT_SYNC_KIND_FULL
             , save      => #{includeText => true}
             }
        , hoverProvider =>
            els_hover_provider:is_enabled()
        , completionProvider =>
            #{ resolveProvider => false
             , triggerCharacters => [<<":">>, <<"#">>, <<"?">>, <<".">>]
             }
        , definitionProvider =>
            els_definition_provider:is_enabled()
        , referencesProvider =>
            els_references_provider:is_enabled()
        , documentHighlightProvider =>
            els_document_highlight_provider:is_enabled()
        , documentSymbolProvider =>
            els_document_symbol_provider:is_enabled()
        , workspaceSymbolProvider =>
            els_workspace_symbol_provider:is_enabled()
        , codeActionProvider =>
            els_code_action_provider:is_enabled()
        , documentFormattingProvider =>
            els_formatting_provider:is_enabled_document()
        , documentRangeFormattingProvider =>
            els_formatting_provider:is_enabled_range()
        , foldingRangeProvider =>
            els_folding_range_provider:is_enabled()
        , implementationProvider =>
            els_implementation_provider:is_enabled()
        , executeCommandProvider =>
            els_execute_command_provider:options()
        , codeLensProvider =>
            els_code_lens_provider:options()
        }
   }.

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
