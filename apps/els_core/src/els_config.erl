-module(els_config).

%% API
-export([
    do_initialize/4,
    initialize/3,
    initialize/4,
    get/1,
    set/2,
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_core.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(DEFAULT_CONFIG_FILE, "erlang_ls.config").
-define(ALTERNATIVE_CONFIG_FILE, "erlang_ls.yaml").
-define(DEFAULT_EXCLUDED_OTP_APPS, [
    "megaco",
    "diameter",
    "snmp",
    "wx"
]).
-define(SERVER, ?MODULE).

%% TODO: Refine names to avoid confusion
-type key() ::
    apps_dirs
    | apps_paths
    | capabilities
    | diagnostics
    | deps_dirs
    | deps_paths
    | include_dirs
    | include_paths
    | lenses
    | otp_path
    | otp_paths
    | otp_apps_exclude
    | plt_path
    | root_uri
    | search_paths
    | code_reload
    | elvis_config_path
    | indexing_enabled
    | compiler_telemetry_enabled
    | refactorerl
    | wrangler
    | edoc_custom_tags
    | providers.

-type path() :: file:filename().
-type state() :: #{
    apps_dirs => [path()],
    apps_paths => [path()],
    lenses => [els_code_lens:lens_id()],
    diagnostics => [els_diagnostics:diagnostic_id()],
    deps_dirs => [path()],
    deps_paths => [path()],
    include_dirs => [path()],
    include_paths => [path()],
    otp_path => path(),
    otp_paths => [path()],
    otp_apps_exclude => [string()],
    plt_path => path(),
    root_uri => uri(),
    search_paths => [path()],
    code_reload => map() | 'disabled',
    indexing_enabled => boolean(),
    compiler_telemetry_enabled => boolean(),
    wrangler => map() | 'notconfigured',
    refactorerl => map() | 'notconfigured',
    providers => map()
}.

-type error_reporting() :: lsp_notification | log.

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec initialize(uri(), map(), map()) -> ok.
initialize(RootUri, Capabilities, InitOptions) ->
    %% https://github.com/erlang-ls/erlang_ls/issues/1060
    initialize(RootUri, Capabilities, InitOptions, _ErrorReporting = log).

-spec initialize(uri(), map(), map(), error_reporting()) -> ok.
initialize(RootUri, Capabilities, InitOptions, ErrorReporting) ->
    RootPath = els_utils:to_list(els_uri:path(RootUri)),
    ConfigPaths = config_paths(RootPath, InitOptions),
    {GlobalConfigPath, MaybeGlobalConfig} = find_config(global_config_paths()),
    {LocalConfigPath, MaybeLocalConfig} = find_config(ConfigPaths),
    ConfigPath =
        case LocalConfigPath of
            undefined ->
                report_missing_config(ErrorReporting),
                GlobalConfigPath;
            _ ->
                LocalConfigPath
        end,
    Config =
        case {MaybeGlobalConfig, MaybeLocalConfig} of
            {{ok, GlobalConfig}, {ok, LocalConfig}} ->
                %% Augment LocalConfig onto GlobalConfig, note that this
                %% is not a deep merge of nested maps.
                maps:merge(GlobalConfig, LocalConfig);
            {{error, Reason}, _} ->
                %% We should not continue if the config is broken, but would
                %% need a bigger initialization refactor to make that work.
                report_broken_config(ErrorReporting, GlobalConfigPath, Reason),
                #{};
            {_, {error, Reason}} ->
                report_broken_config(ErrorReporting, LocalConfigPath, Reason),
                #{}
        end,
    do_initialize(RootUri, Capabilities, InitOptions, {ConfigPath, Config}).

-spec do_initialize(uri(), map(), map(), {undefined | path(), map()}) -> ok.
do_initialize(RootUri, Capabilities, InitOptions, {ConfigPath, Config}) ->
    put(erls_dirs, maps:get("erls_dirs", Config, [])),
    RootPath = els_utils:to_list(els_uri:path(RootUri)),
    OtpPath = maps:get("otp_path", Config, code:root_dir()),
    ?LOG_INFO("OTP Path: ~p", [OtpPath]),
    DepsDirs = maps:get("deps_dirs", Config, []),
    AppsDirs = maps:get("apps_dirs", Config, ["."]),
    IncludeDirs = maps:get("include_dirs", Config, ["include"]),
    ExcludeUnusedIncludes = maps:get("exclude_unused_includes", Config, []),
    Macros = maps:get("macros", Config, []),
    DialyzerPltPath = maps:get("plt_path", Config, undefined),
    OtpAppsExclude = maps:get(
        "otp_apps_exclude",
        Config,
        ?DEFAULT_EXCLUDED_OTP_APPS
    ),
    Lenses = maps:get("lenses", Config, #{}),
    Diagnostics = maps:get("diagnostics", Config, #{}),
    ExcludePathsSpecs = [[OtpPath, "lib", P ++ "*"] || P <- OtpAppsExclude],
    ExcludePaths = els_utils:resolve_paths(ExcludePathsSpecs, true),
    ?LOG_INFO("Excluded OTP Applications: ~p", [OtpAppsExclude]),
    CodeReload = maps:get("code_reload", Config, disabled),
    Runtime = maps:get("runtime", Config, #{}),
    CtRunTest = maps:get("ct-run-test", Config, #{}),
    CodePathExtraDirs = maps:get("code_path_extra_dirs", Config, []),
    ok = add_code_paths(CodePathExtraDirs, RootPath),
    ElvisConfigPath = maps:get("elvis_config_path", Config, undefined),
    IncrementalSync = maps:get("incremental_sync", Config, true),
    Indexing = maps:get("indexing", Config, #{}),
    CompilerTelemetryEnabled =
        maps:get("compiler_telemetry_enabled", Config, false),
    EDocCustomTags = maps:get("edoc_custom_tags", Config, []),

    IndexingEnabled = maps:get(<<"indexingEnabled">>, InitOptions, true),

    RefactorErl = maps:get("refactorerl", Config, notconfigured),
    Providers = maps:get("providers", Config, #{}),
    EdocParseEnabled = maps:get("edoc_parse_enabled", Config, true),

    %% Initialize and start Wrangler
    case maps:get("wrangler", Config, notconfigured) of
        notconfigured ->
            ok = set(wrangler, notconfigured);
        Wrangler ->
            ok = set(wrangler, Wrangler),
            case maps:get("path", Wrangler, notconfigured) of
                notconfigured ->
                    ?LOG_INFO(
                        "Wrangler path is not configured,\n"
                        "                assuming it is installed system-wide."
                    );
                Path ->
                    case code:add_path(Path) of
                        true ->
                            ok;
                        {error, bad_directory} ->
                            ?LOG_INFO(
                                "Wrangler path is configured but\n"
                                "                    not a valid ebin directory: ~p",
                                [Path]
                            )
                    end
            end,
            case application:load(wrangler) of
                ok ->
                    case apply(api_wrangler, start, []) of
                        % Function defined in Wrangler.
                        % Using apply to circumvent tests resulting in 'unknown function'.
                        ok ->
                            ?LOG_INFO("Wrangler started successfully");
                        {error, Reason} ->
                            ?LOG_INFO("Wrangler could not be started: ~p", [Reason])
                    end;
                {error, Reason} ->
                    ?LOG_INFO("Wrangler could not be loaded: ~p", [Reason])
            end
    end,

    %% Passed by the LSP client
    ok = set(root_uri, RootUri),
    %% Read from the configuration file
    ok = set(config_path, ConfigPath),
    ok = set(otp_path, OtpPath),
    ok = set(deps_dirs, DepsDirs),
    ok = set(apps_dirs, AppsDirs),
    ok = set(include_dirs, IncludeDirs),
    ok = set(exclude_unused_includes, ExcludeUnusedIncludes),
    ok = set(macros, Macros),
    ok = set(plt_path, DialyzerPltPath),
    ok = set(code_reload, CodeReload),
    ok = set(providers, Providers),
    ?LOG_INFO("Config=~p", [Config]),
    ok = set(
        runtime,
        maps:merge(
            els_config_runtime:default_config(),
            Runtime
        )
    ),
    ok = set(
        'ct-run-test',
        maps:merge(
            els_config_ct_run_test:default_config(),
            CtRunTest
        )
    ),
    ok = set(elvis_config_path, ElvisConfigPath),
    ok = set(compiler_telemetry_enabled, CompilerTelemetryEnabled),
    ok = set(edoc_custom_tags, EDocCustomTags),
    ok = set(edoc_parse_enabled, EdocParseEnabled),
    ok = set(incremental_sync, IncrementalSync),
    ok = set(
        indexing,
        maps:merge(
            els_config_indexing:default_config(),
            Indexing
        )
    ),
    %% Calculated from the above
    ok = set(apps_paths, project_paths(RootPath, AppsDirs, false)),
    ok = set(deps_paths, project_paths(RootPath, DepsDirs, false)),
    ok = set(include_paths, include_paths(RootPath, IncludeDirs, false)),
    ok = set(otp_paths, otp_paths(OtpPath, false) -- ExcludePaths),
    ok = set(lenses, Lenses),
    ok = set(diagnostics, Diagnostics),
    %% All (including subdirs) paths used to search files with file:path_open/3
    ok = set(
        search_paths,
        lists:append([
            project_paths(RootPath, AppsDirs, true),
            project_paths(RootPath, DepsDirs, true),
            include_paths(RootPath, IncludeDirs, false),
            otp_paths(OtpPath, true)
        ])
    ),
    %% Init Options
    ok = set(capabilities, Capabilities),
    ok = set(indexing_enabled, IndexingEnabled),

    ok = set(refactorerl, RefactorErl),
    ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec get(key()) -> any().
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

-spec set(key(), any()) -> ok.
set(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================

-spec init({}) -> {ok, state()}.
init({}) ->
    {ok, #{}}.

-spec handle_call(any(), any(), state()) ->
    {reply, any(), state()}.
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, Value, State};
handle_call({set, Key, Value}, _From, State0) ->
    State = maps:put(Key, Value, State0),
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec config_paths(path(), map()) -> [path()].
config_paths(
    RootPath,
    #{<<"erlang">> := #{<<"config_path">> := ConfigPath0}}
) ->
    ConfigPath = els_utils:to_list(ConfigPath0),
    lists:append([
        possible_config_paths(ConfigPath),
        possible_config_paths(filename:join([RootPath, ConfigPath])),
        default_config_paths(RootPath)
    ]);
config_paths(RootPath, _Config) ->
    default_config_paths(RootPath).

-spec default_config_paths(path()) -> [path()].
default_config_paths(RootPath) ->
    [
        filename:join([RootPath, ?DEFAULT_CONFIG_FILE]),
        filename:join([RootPath, ?ALTERNATIVE_CONFIG_FILE])
    ].

-spec global_config_paths() -> [path()].
global_config_paths() ->
    GlobalConfigDir = filename:basedir(user_config, "erlang_ls"),
    [
        filename:join([GlobalConfigDir, ?DEFAULT_CONFIG_FILE]),
        filename:join([GlobalConfigDir, ?ALTERNATIVE_CONFIG_FILE])
    ].

%% @doc Bare `Path' as well as with default config file name suffix.
-spec possible_config_paths(path()) -> [path()].
possible_config_paths(Path) ->
    [
        Path,
        filename:join([Path, ?DEFAULT_CONFIG_FILE]),
        filename:join([Path, ?ALTERNATIVE_CONFIG_FILE])
    ].

-spec find_config([path()]) -> {FoundPath, OkConfig | Error} when
    FoundPath :: path() | undefined,
    OkConfig :: {ok, map()},
    Error :: {error, term()}.
find_config(Paths) ->
    case lists:dropwhile(fun(P) -> not filelib:is_regular(P) end, Paths) of
        [FoundPath | _] ->
            {FoundPath, consult_config(FoundPath)};
        _ ->
            {undefined, {ok, #{}}}
    end.

-spec consult_config(path()) -> {ok, map()} | {error, term()}.
consult_config(Path) ->
    Options = [{map_node_format, map}],
    try yamerl:decode_file(Path, Options) of
        [] ->
            ?LOG_WARNING("Using empty configuration from ~s", [Path]),
            {ok, #{}};
        [Config] when is_map(Config) ->
            {ok, Config};
        _ ->
            {error, {syntax_error, Path}}
    catch
        Class:Error ->
            {error, {Class, Error}}
    end.

-spec report_missing_config(error_reporting()) -> ok.
report_missing_config(log) ->
    ?LOG_INFO(
        "The current project is missing an erlang_ls.config file. "
        "Need help configuring Erlang LS for your project? "
        "Visit: https://erlang-ls.github.io/configuration/"
    );
report_missing_config(lsp_notification) ->
    Msg =
        io_lib:format(
            "The current project is missing an erlang_ls.config file. "
            "Need help configuring Erlang LS for your project? "
            "Visit: https://erlang-ls.github.io/configuration/",
            []
        ),
    els_server:send_notification(
        <<"window/showMessage">>,
        #{
            type => ?MESSAGE_TYPE_WARNING,
            message => els_utils:to_binary(Msg)
        }
    ),
    ok.

-spec report_broken_config(
    error_reporting(),
    path(),
    Reason :: term()
) -> ok.
report_broken_config(log, Path, Reason) ->
    ?LOG_ERROR(
        "The erlang_ls.config file at ~s can't be read (~p) "
        "Need help configuring Erlang LS for your project? "
        "Visit: https://erlang-ls.github.io/configuration/",
        [Path, Reason]
    );
report_broken_config(lsp_notification, Path, Reason) ->
    ?LOG_ERROR(
        "Failed to parse configuration file at ~s: ~p",
        [Path, Reason]
    ),
    Msg =
        io_lib:format(
            "The erlang_ls.config file at ~s can't be read "
            "(check logs for details). "
            "Need help configuring Erlang LS for your project? "
            "Visit: https://erlang-ls.github.io/configuration/",
            [Path]
        ),
    els_server:send_notification(
        <<"window/showMessage">>,
        #{
            type => ?MESSAGE_TYPE_WARNING,
            message => els_utils:to_binary(Msg)
        }
    ),
    ok.

-spec include_paths(path(), string(), boolean()) -> [string()].
include_paths(RootPath, IncludeDirs, Recursive) ->
    Paths = [
        els_utils:resolve_paths([[RootPath, Dir]], Recursive)
     || Dir <- IncludeDirs
    ],
    lists:append(Paths).

-spec project_paths(path(), [string()], boolean()) -> [string()].
project_paths(RootPath, Dirs, Recursive) ->
    Paths = [
        els_utils:resolve_paths(
            [
                [RootPath, Dir, "src"],
                [RootPath, Dir, "test"],
                [RootPath, Dir, "include"]
                | [[RootPath, Dir, Src] || Src <- erlang:get(erls_dirs)]
            ],
            Recursive
        )
     || Dir <- Dirs
    ],
    case Recursive of
        false ->
            lists:append(Paths);
        true ->
            Filter = fun(Path) ->
                string:find(Path, "SUITE_data", trailing) =:= nomatch
            end,
            lists:filter(Filter, lists:append(Paths))
    end.

-spec otp_paths(path(), boolean()) -> [string()].
otp_paths(OtpPath, Recursive) ->
    els_utils:resolve_paths(
        [
            [OtpPath, "lib", "*", "src"],
            [OtpPath, "lib", "*", "include"]
        ],
        Recursive
    ).

-spec add_code_paths(
    Dirs :: list(string()),
    RooDir :: string()
) ->
    ok.
add_code_paths(WCDirs, RootDir) ->
    AddADir = fun(ADir) ->
        ?LOG_INFO("Adding code path: ~p", [ADir]),
        true = code:add_path(ADir)
    end,
    AllNames = lists:foldl(
        fun(Elem, AccIn) ->
            AccIn ++ filelib:wildcard(Elem, RootDir)
        end,
        [],
        WCDirs
    ),
    Dirs = [
        [$/ | safe_relative_path(Dir, RootDir)]
     || Name <- AllNames,
        filelib:is_dir([$/ | Dir] = filename:absname(Name, RootDir))
    ],
    lists:foreach(AddADir, Dirs).

-if(?OTP_RELEASE >= 23).
-spec safe_relative_path(
    Dir :: file:name_all(),
    RootDir :: file:name_all()
) ->
    Path :: file:name_all().
safe_relative_path(Dir, RootDir) ->
    filelib:safe_relative_path(Dir, RootDir).
-else.
-spec safe_relative_path(
    FileName :: file:name_all(),
    RootDir :: file:name_all()
) ->
    Path :: file:name_all().
safe_relative_path(Dir, _) ->
    filename:safe_relative_path(Dir).
-endif.
