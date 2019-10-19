-module(erlang_ls_provider).

-behaviour(gen_server).

%% API
-export([ initialize/1
        , handle_request/2
        , start_link/2
        , teardown/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-callback is_enabled()    -> boolean().
-callback setup(config()) -> state().
-callback teardown()      -> ok.

-type config()   :: any().
-type provider() :: erlang_ls_completion_provider
                  | erlang_ls_definition_provider
                  | erlang_ls_references_provider.
-type request()  :: {atom(), map()}.
-type state()    :: any().

-export_type([ config/0
             , provider/0
             , state/0
             ]).

%%==============================================================================
%% External functions
%%==============================================================================

-spec initialize(map()) -> ok.
initialize(Config) ->
  [ start_provider(Provider, Config)
    || Provider <- enabled_providers()
  ],
  ok.

-spec teardown(provider(), config()) -> ok.
teardown(Provider, Config) ->
  Provider:teardown(Config).

-spec start_link(provider(), config()) -> {ok, pid()}.
start_link(Provider, Config) ->
  Args = #{provider => Provider, config => Config},
  gen_server:start_link({local, Provider}, ?MODULE, Args, []).

-spec handle_request(provider(), request()) -> any().
handle_request(Provider, Request) ->
  gen_server:call(Provider, {handle_request, Provider, Request}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(#{provider := atom(), config := map()}) -> {ok, state()}.
init(#{provider := Provider, config := Config}) ->
  State = Provider:setup(Config),
  {ok, State}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {reply, any(), state()}.
handle_call({handle_request, Provider, Request}, _From, State) ->
  {Reply, NewState} = Provider:handle_request(Request, State),
  {reply, Reply, NewState}.

-spec handle_cast(any(), any()) ->
  {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec start_provider(provider(), config()) -> ok.
start_provider(Provider, Config) ->
  supervisor:start_child(erlang_ls_providers_sup, [Provider, Config]),
  ok.

-spec providers() -> [provider()].
providers() ->
  [ erlang_ls_completion_provider
  , erlang_ls_definition_provider
  , erlang_ls_references_provider
  ].

-spec enabled_providers() -> [provider()].
enabled_providers() ->
  [Provider || Provider <- providers(), Provider:is_enabled()].
