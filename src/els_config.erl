-module(els_config).

%% API
-export([ initialize/3
        , get/1
        , set/2
        , start_link/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(DEFAULT_CONFIG_PATH, "erlang_ls.config").
-define( DEFAULT_EXCLUDED_OTP_APPS
       , [ "megaco"
         , "diameter"
         , "snmp"
         , "wx"
         ]
       ).
-define(SERVER, ?MODULE).

%% TODO: Refine names to avoid confusion
-type key()   :: apps_dirs
               | apps_paths
               | capabilities
               | deps_dirs
               | deps_paths
               | include_dirs
               | include_paths
               | otp_path
               | otp_paths
               | otp_apps_exclude
               | plt_path
               | root_uri
               | search_paths.
-type path()  :: file:filename().
-type state() :: #{ apps_dirs         => [path()]
                  , apps_paths        => [path()]
                  , deps_dirs         => [path()]
                  , deps_paths        => [path()]
                  , include_dirs      => [path()]
                  , include_paths     => [path()]
                  , otp_path          => path()
                  , otp_paths         => [path()]
                  , otp_apps_exclude  => [string()]
                  , plt_path          => path()
                  , root_uri          => uri()
                  , search_paths      => [path()]
                  }.

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec initialize(uri(), map(), map()) -> ok.
initialize(RootUri, Capabilities, InitOptions) ->
  RootPath = binary_to_list(els_uri:path(RootUri)),
  Config   = consult_config(filename:join([ RootPath
                                          , config_path(InitOptions)
                                          ])),
  OtpPath         = maps:get("otp_path", Config, code:root_dir()),
  DepsDirs        = maps:get("deps_dirs", Config, []),
  AppsDirs        = maps:get("apps_dirs", Config, ["."]),
  IncludeDirs     = maps:get("include_dirs", Config, ["include"]),
  DialyzerPltPath = maps:get("plt_path", Config, undefined),
  OtpAppsExclude  = maps:get( "otp_apps_exclude"
                            , Config
                            , ?DEFAULT_EXCLUDED_OTP_APPS
                            ),
  ExcludePathsSpecs = [[OtpPath, "lib", P ++ "*"] || P <- OtpAppsExclude],
  ExcludePaths = els_utils:resolve_paths(ExcludePathsSpecs, RootPath, true),
  lager:info("Excluded OTP Applications: ~p", [OtpAppsExclude]),

  %% Passed by the LSP client
  ok = set(root_uri      , RootUri),
  %% Read from the erlang_ls.config file
  ok = set(otp_path      , OtpPath),
  ok = set(deps_dirs     , DepsDirs),
  ok = set(apps_dirs     , AppsDirs),
  ok = set(include_dirs  , IncludeDirs),
  ok = set(plt_path      , DialyzerPltPath),
  %% Calculated from the above
  ok = set(apps_paths    , project_paths(RootPath, AppsDirs, false)),
  ok = set(deps_paths    , project_paths(RootPath, DepsDirs, false)),
  ok = set(include_paths , include_paths(RootPath, IncludeDirs, false)),
  ok = set(otp_paths     , otp_paths(OtpPath, false) -- ExcludePaths),
  %% All (including subdirs) paths used to search files with file:path_open/3
  ok = set( search_paths
          , lists:append([ project_paths(RootPath, AppsDirs, true)
                         , project_paths(RootPath, DepsDirs, true)
                         , otp_paths(OtpPath, true)
                         ])
          ),
  %% Init Options
  ok = set(capabilities  , Capabilities),
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

-spec config_path(map()) -> els_uri:path().
config_path(#{<<"erlang">> := #{<<"config_path">> := ConfigPath}}) ->
  ConfigPath;
config_path(_) ->
  ?DEFAULT_CONFIG_PATH.

-spec consult_config(els_uri:path()) -> map().
consult_config(Path) ->
  lager:info("Reading config file. path=~p", [Path]),
  Options = [{map_node_format, map}],
  try yamerl:decode_file(Path, Options) of
      [] -> #{};
      [Config] -> Config
  catch
    Class:Error ->
      lager:warning( "Could not read config file: path=~p class=~p error=~p"
                   , [Path, Class, Error]),
      #{}
  end.

-spec include_paths(path(), string(), boolean()) -> [string()].
include_paths(RootPath, IncludeDirs, Recursive) ->
  Paths = [ els_utils:resolve_paths([[RootPath, Dir]], RootPath, Recursive)
            || Dir <- IncludeDirs
          ],
  lists:append(Paths).

-spec project_paths(path(), [string()], boolean()) -> [string()].
project_paths(RootPath, Dirs, Recursive) ->
  Paths = [ els_utils:resolve_paths( [ [RootPath, Dir, "src"]
                                     , [RootPath, Dir, "test"]
                                     , [RootPath, Dir, "include"]
                                     ]
                                   , RootPath
                                   , Recursive
                                   )
            || Dir <- Dirs
          ],
  lists:append(Paths).

-spec otp_paths(path(), boolean()) -> [string()].
otp_paths(OtpPath, Recursive) ->
  els_utils:resolve_paths( [ [OtpPath, "lib", "*", "src"]
                           , [OtpPath, "lib", "*", "include"]
                           ]
                         , OtpPath
                         , Recursive
                         ).
