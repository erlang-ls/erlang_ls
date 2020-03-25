%%==============================================================================
%% Providers Supervisor
%%==============================================================================
-module(els_providers_sup).

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
-type provider_spec() :: #{ id => els_provider:provider()
                          , start => {els_provider:provider(), start_link, []}
                          , shutdown => brutal_kill
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
  ChildSpecs = [provider_specs(P) || P <- els_provider:enabled_providers()],
  {ok, {SupFlags, ChildSpecs}}.

-spec provider_specs(els_provider:provider()) -> provider_spec().
provider_specs(Provider) ->
  #{ id => Provider
   , start => {els_provider, start_link, [Provider]}
   , shutdown => brutal_kill
   }.
