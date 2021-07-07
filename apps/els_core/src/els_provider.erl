-module(els_provider).

%% API
-export([ handle_request/2
        , start_link/1
        , available_providers/0
        , enabled_providers/0
        , cancel_request/2
        ]).

-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

-callback is_enabled() -> boolean().
-callback init() -> any().
-callback handle_request(request(), any()) -> {any(), any()}.
-callback handle_info(any(), any()) -> any().
-callback cancel_request(pid(), any()) -> any().
-optional_callbacks([init/0, handle_info/2, cancel_request/2]).

-type config()   :: any().
-type provider() :: els_completion_provider
                  | els_definition_provider
                  | els_document_symbol_provider
                  | els_hover_provider
                  | els_references_provider
                  | els_formatting_provider
                  | els_document_highlight_provider
                  | els_workspace_symbol_provider
                  | els_folding_range_provider
                  | els_implementation_provider
                  | els_code_action_provider
                  | els_general_provider
                  | els_code_lens_provider
                  | els_execute_command_provider
                  | els_rename_provider
                  | els_bsp_provider.
-type request()  :: {atom() | binary(), map()}.
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
  gen_server:call(Provider, {handle_request, Request}, infinity).

-spec cancel_request(provider(), pid()) -> any().
cancel_request(Provider, Job) ->
  gen_server:cast(Provider, {cancel_request, Job}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(els_provider:provider()) -> {ok, state()}.
init(Provider) ->
  ?LOG_INFO("Starting provider ~p", [Provider]),
  InternalState = case erlang:function_exported(Provider, init, 0) of
                    true ->
                      Provider:init();
                    false ->
                      #{}
                  end,
  {ok, #{provider => Provider, internal_state => InternalState}}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {reply, any(), state()}.
handle_call({handle_request, Request}, _From, State) ->
  #{internal_state := InternalState, provider := Provider} = State,
  {Reply, NewInternalState} = Provider:handle_request(Request, InternalState),
  {reply, Reply, State#{internal_state => NewInternalState}}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({cancel_request, Job}, State) ->
  #{internal_state := InternalState, provider := Provider} = State,
  case erlang:function_exported(Provider, cancel_request, 2) of
    true ->
      NewInternalState = Provider:cancel_request(Job, InternalState),
      {noreply, State#{internal_state => NewInternalState}};
    false ->
      {noreply, State}
  end.

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

-spec available_providers() -> [provider()].
available_providers() ->
  [ els_completion_provider
  , els_definition_provider
  , els_document_symbol_provider
  , els_hover_provider
  , els_references_provider
  , els_formatting_provider
  , els_document_highlight_provider
  , els_workspace_symbol_provider
  , els_folding_range_provider
  , els_implementation_provider
  , els_code_action_provider
  , els_general_provider
  , els_code_lens_provider
  , els_execute_command_provider
  , els_diagnostics_provider
  , els_rename_provider
  , els_bsp_provider
  ].

-spec enabled_providers() -> [provider()].
enabled_providers() ->
  [Provider || Provider <- available_providers(), Provider:is_enabled()].
