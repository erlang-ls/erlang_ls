-module(els_general_provider).

-behaviour(els_provider).
-export([
    default_providers/0,
    enabled_providers/0,
    handle_request/1
]).

-export([server_capabilities/0]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================

-type server_capabilities() :: map().
-type initialize_request() :: {initialize, initialize_params()}.
-type initialize_params() :: #{
    processId := number() | null,
    rootPath => binary() | null,
    rootUri := uri() | null,
    initializationOptions => any(),
    capabilities := client_capabilities(),
    trace =>
        off
        | messages
        | verbose,
    workspaceFolders =>
        [workspace_folder()]
        | null
}.
-type initialize_result() :: #{capabilities => server_capabilities()}.
-type initialized_request() :: {initialized, initialized_params()}.
-type initialized_params() :: #{}.
-type initialized_result() :: null.
-type shutdown_request() :: {shutdown, shutdown_params()}.
-type shutdown_params() :: #{}.
-type shutdown_result() :: null.
-type exit_request() :: {exit, exit_params()}.
-type exit_params() :: #{status => atom()}.
-type exit_result() :: null.
-type provider_id() :: string().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(
    initialize_request()
    | initialized_request()
    | shutdown_request()
    | exit_request()
) ->
    {response,
        initialize_result()
        | initialized_result()
        | shutdown_result()
        | exit_result()}.
handle_request({initialize, Params}) ->
    #{
        <<"rootUri">> := RootUri0,
        <<"capabilities">> := Capabilities
    } = Params,
    RootUri =
        case RootUri0 of
            null ->
                {ok, Cwd} = file:get_cwd(),
                els_uri:uri(els_utils:to_binary(Cwd));
            _ ->
                RootUri0
        end,
    InitOptions =
        case maps:get(<<"initializationOptions">>, Params, #{}) of
            InitOptions0 when is_map(InitOptions0) ->
                InitOptions0;
            _ ->
                #{}
        end,
    ok = els_config:initialize(RootUri, Capabilities, InitOptions, true),
    {response, server_capabilities()};
handle_request({initialized, _Params}) ->
    RootUri = els_config:get(root_uri),
    NodeName = els_distribution_server:node_name(
        <<"erlang_ls">>,
        filename:basename(RootUri)
    ),
    register_capabilities(),
    els_distribution_server:start_distribution(NodeName),
    ?LOG_INFO("Started distribution for: [~p]", [NodeName]),
    els_indexing:maybe_start(),
    {response, null};
handle_request({shutdown, _Params}) ->
    {response, null};
handle_request({exit, #{status := Status}}) ->
    ?LOG_INFO("Language server stopping..."),
    ExitCode =
        case Status of
            shutdown -> 0;
            _ -> 1
        end,
    els_utils:halt(ExitCode),
    {response, null}.

%%==============================================================================
%% API
%%==============================================================================

%% @doc Give all available providers
-spec available_providers() -> [provider_id()].
available_providers() ->
    [
        "text-document-sync",
        "hover",
        "completion",
        "signature-help",
        "definition",
        "references",
        "document-highlight",
        "document-symbol",
        "workspace-symbol",
        "code-action",
        "document-formatting",
        "document-range-formatting",
        "document-on-type-formatting",
        "folding-range",
        "implementation",
        "execute-command",
        "code-lens",
        "rename",
        "call-hierarchy",
        "semantic-tokens"
    ].

%% @doc Give the list of all providers enabled by default.
-spec default_providers() -> [provider_id()].
default_providers() ->
    available_providers() --
        [
            "document-range-formatting",
            %% NOTE: because erlang_ls does not send incremental document changes
            %%       via `textDocument/didChange', this kind of formatting does not
            %%       make sense.
            "document-on-type-formatting",
            %% Signature help is experimental.
            "signature-help"
        ].

%% @doc Give the list of all providers enabled by the current configuration.
-spec enabled_providers() -> [provider_id()].
enabled_providers() ->
    Config = els_config:get(providers),
    Default = default_providers(),
    Enabled = maps:get("enabled", Config, []),
    Disabled = maps:get("disabled", Config, []),
    lists:usort((Default ++ valid(Enabled)) -- valid(Disabled)).

%% @doc Give the LSP server capabilities map for all capabilities enabled by
%% the current configuration.
-spec server_capabilities() -> server_capabilities().
server_capabilities() ->
    {ok, Version} = application:get_key(?APP, vsn),
    AvailableCapabilities =
        #{
            textDocumentSync =>
                els_text_synchronization_provider:options(),
            hoverProvider => true,
            completionProvider =>
                #{
                    resolveProvider => true,
                    triggerCharacters =>
                        els_completion_provider:trigger_characters()
                },
            signatureHelpProvider =>
                #{
                    triggerCharacters =>
                        els_signature_help_provider:trigger_characters()
                },
            definitionProvider => true,
            referencesProvider => true,
            documentHighlightProvider => true,
            documentSymbolProvider => true,
            workspaceSymbolProvider => true,
            codeActionProvider => true,
            documentFormattingProvider => true,
            documentRangeFormattingProvider => false,
            foldingRangeProvider => true,
            implementationProvider => true,
            executeCommandProvider =>
                els_execute_command_provider:options(),
            codeLensProvider =>
                els_code_lens_provider:options(),
            renameProvider =>
                els_rename_provider:options(),
            callHierarchyProvider => true,
            semanticTokensProvider =>
                #{
                    legend =>
                        #{
                            tokenTypes => wrangler_handler:semantic_token_types(),
                            tokenModifiers => wrangler_handler:semantic_token_modifiers()
                        },
                    range => false,
                    full => wrangler_handler:is_enabled()
                }
        },
    EnabledProviders = enabled_providers(),
    ConfiguredCapabilities =
        maps:filter(
            fun(Provider, _Config) ->
                lists:member(provider_id(Provider), EnabledProviders)
            end,
            AvailableCapabilities
        ),
    #{
        capabilities => ConfiguredCapabilities,
        serverInfo =>
            #{
                name => <<"Erlang LS">>,
                version => els_utils:to_binary(Version)
            }
    }.

-spec register_capabilities() -> ok.
register_capabilities() ->
    Methods = [<<"didChangeWatchedFiles">>],
    ClientCapabilities = els_config:get(capabilities),
    Registrations = [
        dynamic_registration_options(Method)
     || Method <- Methods, is_dynamic_registration_enabled(Method, ClientCapabilities)
    ],
    case Registrations of
        [] ->
            ?LOG_INFO("Skipping dynamic capabilities registration");
        _ ->
            Params = #{registrations => Registrations},
            els_server:send_request(<<"client/registerCapability">>, Params)
    end.

-spec is_dynamic_registration_enabled(binary(), map()) -> boolean().
is_dynamic_registration_enabled(Method, ClientCapabilities) ->
    maps:get(
        <<"dynamicRegistration">>,
        maps:get(Method, maps:get(<<"workspace">>, ClientCapabilities, #{}), #{}),
        false
    ).

-spec dynamic_registration_options(binary()) -> map().
dynamic_registration_options(<<"didChangeWatchedFiles">>) ->
    RootPath = els_uri:path(els_config:get(root_uri)),
    GlobPattern = filename:join([RootPath, "**", "*.{e,h}rl"]),
    #{
        id => <<"workspace/didChangeWatchedFiles">>,
        method => <<"workspace/didChangeWatchedFiles">>,
        registerOptions => #{
            watchers => [#{globPattern => GlobPattern}]
        }
    }.

-spec valid([any()]) -> [provider_id()].
valid(ProviderIds) ->
    {Valid, Invalid} = lists:partition(fun is_valid_provider_id/1, ProviderIds),
    case Invalid of
        [] ->
            ok;
        _ ->
            Fmt = "Discarding invalid providers in config file: ~p",
            Args = [Invalid],
            Msg = lists:flatten(io_lib:format(Fmt, Args)),
            ?LOG_WARNING(Msg),
            els_server:send_notification(
                <<"window/showMessage">>,
                #{
                    type => ?MESSAGE_TYPE_WARNING,
                    message => els_utils:to_binary(Msg)
                }
            )
    end,
    Valid.

-spec is_valid_provider_id(any()) -> boolean().
is_valid_provider_id(ProviderId) ->
    lists:member(ProviderId, available_providers()).

-spec provider_id(atom()) -> provider_id().
provider_id(textDocumentSync) -> "text-document-sync";
provider_id(completionProvider) -> "completion";
provider_id(hoverProvider) -> "hover";
provider_id(signatureHelpProvider) -> "signature-help";
provider_id(definitionProvider) -> "definition";
provider_id(referencesProvider) -> "references";
provider_id(documentHighlightProvider) -> "document-highlight";
provider_id(documentSymbolProvider) -> "document-symbol";
provider_id(workspaceSymbolProvider) -> "workspace-symbol";
provider_id(codeActionProvider) -> "code-action";
provider_id(documentFormattingProvider) -> "document-formatting";
provider_id(documentRangeFormattingProvider) -> "document-range-formatting";
provider_id(documentOnTypeFormattingProvider) -> "document-on-type-formatting";
provider_id(foldingRangeProvider) -> "folding-range";
provider_id(implementationProvider) -> "implementation";
provider_id(executeCommandProvider) -> "execute-command";
provider_id(codeLensProvider) -> "code-lens";
provider_id(renameProvider) -> "rename";
provider_id(callHierarchyProvider) -> "call-hierarchy";
provider_id(semanticTokensProvider) -> "semantic-tokens".
