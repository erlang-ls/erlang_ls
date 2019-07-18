%%==============================================================================
%% Top Level Supervisor
%%==============================================================================
-module(erlang_ls_sup).

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
  ChildSpecs = [ #{ id       => erlang_ls_buffer_server
                  , start    => {erlang_ls_buffer_server, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id       => erlang_ls_buffer_sup
                  , start    => {erlang_ls_buffer_sup, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id       => erlang_ls_indexer
                  , start    => {erlang_ls_indexer, start_link, []}
                  , shutdown => brutal_kill
                  }
               ],
  {ok, {SupFlags, ChildSpecs}}.
