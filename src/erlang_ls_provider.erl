-module(erlang_ls_provider).

-behaviour(gen_server).

-callback is_enabled()    -> boolean().
-callback setup(config()) -> state().
-callback teardown()      -> ok.

-export([ enabled_providers/0
        , is_enabled/1
        , providers/0
        , teardown/2
        ]).

-type config()   :: any().
-type provider() :: erlang_ls_definition_provider.
-type request()  :: any().
-type state()    :: any().
-export_type([ config/0
             , provider/0
             , state/0
             ]).

%% API
-export([ start_link/2
        , start_provider/2
        , handle_request/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-spec providers() -> [provider()].
providers() ->
  [erlang_ls_definition_provider].

-spec enabled_providers() -> [provider()].
enabled_providers() ->
  [Provider || Provider <- providers(), is_enabled(Provider)].

-spec is_enabled(provider()) -> boolean().
is_enabled(Provider) ->
  Provider:is_enabled().

-spec teardown(provider(), config()) -> ok.
teardown(Provider, Config) ->
  Provider:teardown(Config).

-spec start_link(provider(), config()) -> {ok, pid()}.
start_link(Provider, Config) ->
  Args = [#{provider => Provider, config => Config}],
  gen_server:start_link({local, Provider}, ?MODULE, Args, []).

-spec start_provider(provider(), config()) -> ok.
start_provider(Provider, Config) ->
  supervisor:start_child(erlang_ls_providers_sup, [Provider, Config]),
  ok.

-spec handle_request(provider(), request()) -> any().
handle_request(Provider, Request) ->
  gen_server:call(Provider, {handle_request, Request}).

-spec init([#{provider := atom(), config := map()}]) -> {ok, state()}.
init([#{provider := Provider, config := Config}]) ->
  State = Provider:setup(Config),
  {ok, State}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {reply, any(), state()}.
handle_call({handle_request, Provider, Request, Params}, _From, State) ->
  {Reply, NewState} = Provider:handle_request(Request, Params, State),
  {reply, Reply, NewState}.

-spec handle_cast(any(), any()) ->
  {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.
