%%==============================================================================
%% Top Level Supervisor
%%==============================================================================
-module(els_sup).

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
%% Supervisor callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => rest_for_one
              , intensity => 5
              , period    => 60
              },
  {ok, Transport} = application:get_env(erlang_ls, transport),
  ChildSpecs = [ #{ id       => els_server
                  , start    => {els_server, start_link, [Transport]}
                  }
               , #{ id       => els_config
                  , start    => {els_config, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id       => els_indexer
                  , start    => {els_indexer, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id    => els_providers_sup
                  , start => {els_providers_sup, start_link, []}
                  , type  => supervisor
                  }
               ],
  {ok, {SupFlags, ChildSpecs}}.
