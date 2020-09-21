%%==============================================================================
%% @doc Erlang DAP Provider Behaviour
%% @end
%%==============================================================================
-module(els_dap_provider).

%% API
-export([ handle_request/2
        , start_link/1
        , available_providers/0
        , enabled_providers/0
        ]).

-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

-callback is_enabled() -> boolean().
-callback init() -> any().
-callback handle_request(request(), any()) -> {any(), any()}.
-callback handle_info(any(), any()) -> any().
-optional_callbacks([init/0, handle_info/2]).

-type config()   :: any().
-type provider() :: els_dap_general_provider.
-type request()  :: {atom(), map()}.
-type state()    :: #{ provider := provider()
                     , internal_state := any()
                     }.

-export_type([ config/0
             , provider/0
             , request/0
             , state/0
             ]).

%%==============================================================================
%% External functions
%%==============================================================================

-spec start_link(provider()) -> {ok, pid()}.
start_link(Provider) ->
  gen_server:start_link({local, Provider}, ?MODULE, Provider, []).

-spec handle_request(provider(), request()) -> any().
handle_request(Provider, Request) ->
  gen_server:call(Provider, {handle_request, Provider, Request}, infinity).

-spec available_providers() -> [provider()].
available_providers() ->
  [ els_dap_general_provider
  ].

-spec enabled_providers() -> [provider()].
enabled_providers() ->
  [Provider || Provider <- available_providers(), Provider:is_enabled()].

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(els_provider:provider()) -> {ok, state()}.
init(Provider) ->
  lager:info("Starting provider ~p", [Provider]),
  InternalState = case erlang:function_exported(Provider, init, 0) of
                    true ->
                      Provider:init();
                    false ->
                      #{}
                  end,
  {ok, #{provider => Provider, internal_state => InternalState}}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {reply, any(), state()}.
handle_call({handle_request, Provider, Request}, _From, State) ->
  #{internal_state := InternalState} = State,
  {Reply, NewInternalState} = Provider:handle_request(Request, InternalState),
  {reply, Reply, State#{internal_state => NewInternalState}}.

-spec handle_cast(any(), state()) ->
  {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) ->
  {noreply, state()}.
handle_info(Request, State) ->
  #{provider := Provider, internal_state := InternalState} = State,
  case erlang:function_exported(Provider, handle_info, 2) of
    true ->
      NewInternalState = Provider:handle_info(Request, InternalState),
      {noreply, State#{internal_state => NewInternalState}};
    false ->
      {noreply, State}
  end.
