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
%% Macros
%%==============================================================================
-define(DEFAULT_CONFIG_PATH, "erlang_ls.config").
-define(SERVER, ?MODULE).

-type key()   :: root_uri | deps_dirs | otp_path.
-type path()   :: file:filename().
-type state() :: #{ deps_dirs => [path()]
                  , otp_path  => path()
                  , root_uri  => erlang_ls_uri:uri()
                  }.


%%==============================================================================
%% Exported functions
%%==============================================================================

-spec initialize(erlang_ls_uri:uri(), map()) -> map().
initialize(RootUri, InitOptions) ->
  Config = consult_config(filename:join([ erlang_ls_uri:path(RootUri)
                                        , config_path(InitOptions)
                                        ])),
  OtpPath = maps:get("otp_path", Config, code:root_dir()),
  DepsDirs = maps:get("deps_dirs", Config, []),
  ok = set(root_uri, RootUri),
  ok = set(otp_path, OtpPath),
  ok = set(deps_dirs, DepsDirs),
  Config.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec get(key()) -> {ok, any() | undefined}.
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
  {reply, ok, state()}.
handle_call({get, Key}, _From, State) ->
  Value = maps:get(Key, State, undefined),
  {reply, {ok, Value}, State};
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
