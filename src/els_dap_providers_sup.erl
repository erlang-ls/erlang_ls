%%==============================================================================
%% @doc Erlang DAP Providers' Supervisor
%% @end
%%==============================================================================
-module(els_dap_providers_sup).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(supervisor).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ start_link/0 ]).

%% Supervisor Callbacks
-export([ init/1 ]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type provider_spec() ::
        #{ id := els_dap_provider:provider()
         , start := {els_dap_provider, start_link, [els_dap_provider:provider()]}
         , shutdown := brutal_kill
         }.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => one_for_one
              , intensity => 1
              , period    => 5
              },
  ChildSpecs = [provider_specs(P) || P <- els_dap_provider:enabled_providers()],
  {ok, {SupFlags, ChildSpecs}}.

-spec provider_specs(els_dap_provider:provider()) -> provider_spec().
provider_specs(Provider) ->
  #{ id => Provider
   , start => {els_dap_provider, start_link, [Provider]}
   , shutdown => brutal_kill
   }.
