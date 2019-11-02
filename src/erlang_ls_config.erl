-module(erlang_ls_config).

%% API
-export([ initialize/2
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
-define(SERVER, ?MODULE).

%% TODO: Refine names to avoid confusion
-type key()   :: app_paths
               | deps_dirs
               | deps_paths
               | include_dirs
               | include_paths
               | otp_path
               | otp_paths
               | root_uri.
-type path()  :: file:filename().
-type state() :: #{ app_paths     => [path()]
                  , deps_dirs     => [path()]
                  , deps_paths    => [path()]
                  , include_dirs  => [path()]
                  , include_paths => [path()]
                  , otp_path      => path()
                  , otp_paths     => [path()]
                  , root_uri      => uri()
                  }.

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec initialize(uri(), map()) -> ok.
initialize(RootUri, InitOptions) ->
  Config = consult_config(filename:join([ erlang_ls_uri:path(RootUri)
                                        , config_path(InitOptions)
                                        ])),
  OtpPath = maps:get("otp_path", Config, code:root_dir()),
  DepsDirs = maps:get("deps_dirs", Config, []),
  IncludeDirs = maps:get("include_dirs", Config, ["include"]),
  %% Passed by the LSP client
  ok = set(root_uri      , RootUri),
  %% Read from the erlang_ls.config file
  ok = set(otp_path      , OtpPath),
  ok = set(deps_dirs     , DepsDirs),
  ok = set(include_dirs  , IncludeDirs),
  %% Calculated from the above
  ok = set(app_paths     , app_paths(RootUri)),
  ok = set(deps_paths    , deps_paths(RootUri, DepsDirs)),
  ok = set(include_paths , include_paths(RootUri, IncludeDirs)),
  ok = set(otp_paths     , otp_paths(OtpPath)),
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
      lager:warning( "Could not read config file: path=~p class=~p error=~p"
                   , [Path, Class, Error]),
      #{}
  end.

-spec app_paths(uri()) -> [string()].
app_paths(RootUri) ->
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  resolve_paths( [ [RootPath, "src"]
                 , [RootPath, "test"]
                 , [RootPath, "include"]
                 ]).

-spec include_paths(uri(), string()) -> [string()].
include_paths(RootUri, IncludeDirs) ->
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  Paths = [resolve_paths( [ [RootPath, Dir] ]) || Dir <- IncludeDirs],
  lists:append(Paths).

-spec deps_paths(uri(), [string()]) -> [string()].
deps_paths(RootUri, DepsDirs) ->
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  Paths = [ resolve_paths( [ [RootPath, Dir, "src"]
                           , [RootPath, Dir, "test"]
                           , [RootPath, Dir, "include"]
                           ])
            || Dir <- DepsDirs
          ],
  lists:append(Paths).

-spec otp_paths(string()) -> [string()].
otp_paths(OtpPath) ->
  resolve_paths( [ [OtpPath, "lib", "*", "src"]
                 , [OtpPath, "lib", "*", "include"]
                 ]).

-spec resolve_paths([[string()]]) -> [[string()]].
resolve_paths(PathSpecs) ->
  lists:append([resolve_path(PathSpec) || PathSpec <- PathSpecs]).

-spec resolve_path([string()]) -> [string()].
resolve_path(PathSpec) ->
  Path = filename:join(PathSpec),
  Paths = [[P | subdirs(P)] || P <- filelib:wildcard(Path)],
  lists:append(Paths).

%% Returns all subdirectories for the provided path
-spec subdirs(string()) -> [string()].
subdirs(Path) ->
  subdirs(Path, []).

-spec subdirs(string(), [string()]) -> [string()].
subdirs(Path, Subdirs) ->
  case file:list_dir(Path) of
    {ok, Files}     -> subdirs_(Path, Files, Subdirs);
    {error, enoent} -> Subdirs
  end.

-spec subdirs_(string(), [string()], [string()]) -> [string()].
subdirs_(Path, Files, Subdirs) ->
  Fold = fun(F, Acc) ->
             FullPath = filename:join([Path, F]),
             case filelib:is_dir(FullPath) of
               true  -> subdirs(FullPath, [FullPath | Acc]);
               false -> Acc
             end
         end,
  lists:foldl(Fold, Subdirs, Files).
