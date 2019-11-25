-module(els_provider).

-behaviour(gen_server).

%% API
-export([ initialize/0
        , handle_request/2
        , start_link/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-callback is_enabled() -> boolean().

-type config()   :: any().
-type provider() :: els_completion_provider
                  | els_definition_provider
                  | els_document_symbol_provider
                  | els_hover_provider
                  | els_references_provider
                  | els_document_highlight_provider
                  | els_workspace_symbol_provider.
-type request()  :: {atom(), map()}.
-type state()    :: any().

-export_type([ config/0
             , provider/0
             , state/0
             ]).

%%==============================================================================
%% External functions
%%==============================================================================

-spec initialize() -> ok.
initialize() ->
  [ start_provider(Provider) || Provider <- enabled_providers() ],
  ok.

-spec start_link(provider()) -> {ok, pid()}.
start_link(Provider) ->
  gen_server:start_link({local, Provider}, ?MODULE, none, []).

-spec handle_request(provider(), request()) -> any().
handle_request(Provider, Request) ->
  gen_server:call(Provider, {handle_request, Provider, Request}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(none) -> {ok, state()}.
init(none) ->
  {ok, #{}}.

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

-spec start_provider(provider()) -> ok.
start_provider(Provider) ->
  supervisor:start_child(els_providers_sup, [Provider]),
  ok.

%% TODO: This could be moved to the supervisor
-spec providers() -> [provider()].
providers() ->
  [ els_completion_provider
  , els_definition_provider
  , els_document_symbol_provider
  , els_hover_provider
  , els_references_provider
  , els_document_highlight_provider
  , els_workspace_symbol_provider
  ].

-spec enabled_providers() -> [provider()].
enabled_providers() ->
  [Provider || Provider <- providers(), Provider:is_enabled()].
