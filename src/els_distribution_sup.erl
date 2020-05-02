%%==============================================================================
%% Distribution Supervisor
%% It supervises the communication via Erlang distribution between the
%% Language Server Node and the Runtime Node.
%%==============================================================================
-module(els_distribution_sup).

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
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==============================================================================
%% supervisors callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => one_for_one
              , intensity => 5
              , period    => 60
              },
  ChildSpecs = [ #{ id    => els_build_server
                  , start => {els_build_server, start_link, []}
                  }
               , #{ id    => els_group_leader_sup
                  , start => {els_group_leader_sup, start_link, []}
                  , type  => supervisor
                  }
               ],
  {ok, {SupFlags, ChildSpecs}}.
