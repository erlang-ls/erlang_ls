%%==============================================================================
%% @doc Erlang DAP Provider Behaviour
%% @end
%%==============================================================================
-module(els_dap_provider).

%% API
-export([
    handle_request/2,
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

-callback is_enabled() -> boolean().
-callback init() -> any().
-callback handle_request(request(), any()) -> {any(), any()}.
-callback handle_info(any(), any()) -> any().
-optional_callbacks([init/0, handle_info/2]).

-type config() :: any().
-type provider() :: els_dap_general_provider.
-type request() :: {binary(), map()}.
-type state() :: #{internal_state := any()}.

-export_type([
    config/0,
    provider/0,
    request/0,
    state/0
]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% External functions
%%==============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, unused, []).

-spec handle_request(provider(), request()) -> any().
handle_request(Provider, Request) ->
    gen_server:call(?SERVER, {handle_request, Provider, Request}, infinity).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(unused) -> {ok, state()}.
init(unused) ->
    ?LOG_INFO("Starting DAP provider", []),
    InternalState = els_dap_general_provider:init(),
    {ok, #{internal_state => InternalState}}.

-spec handle_call(any(), {pid(), any()}, state()) ->
    {reply, any(), state()}.
handle_call({handle_request, Provider, Request}, _From, State) ->
    #{internal_state := InternalState} = State,
    {Reply, NewInternalState} =
        Provider:handle_request(Request, InternalState),
    {reply, Reply, State#{internal_state => NewInternalState}}.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) ->
    {noreply, state()}.
handle_info(Request, State) ->
    #{internal_state := InternalState} = State,
    NewInternalState =
        els_dap_general_provider:handle_info(Request, InternalState),
    {noreply, State#{internal_state => NewInternalState}}.
