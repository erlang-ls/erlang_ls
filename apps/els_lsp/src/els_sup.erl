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
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

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
  SupFlags = #{ strategy  => rest_for_one
              , intensity => 5
              , period    => 60
              },
  {ok, Vsn} = application:get_key(vsn),
  ?LOG_INFO("Starting session (version ~p)", [Vsn]),
  restrict_stdio_access(),
  ChildSpecs = [ #{ id       => els_config
                  , start    => {els_config, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id       => els_db_server
                  , start    => {els_db_server, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id    => els_background_job_sup
                  , start => {els_background_job_sup, start_link, []}
                  , type  => supervisor
                  }
               , #{ id    => els_distribution_sup
                  , start => {els_distribution_sup, start_link, []}
                  , type  => supervisor
                  }
               , #{ id    => els_snippets_server
                  , start => {els_snippets_server, start_link, []}
                  }
               , #{ id       => els_provider
                  , start    => {els_provider, start_link, []}
                  }
               , #{ id       => els_server
                  , start    => {els_server, start_link, []}
                  }
               ],
  {ok, {SupFlags, ChildSpecs}}.

%% @doc Restrict access to standard I/O
%%
%% Sets the `io_device' application variable to the current group
%% leaders and replaces the group leader process of this supervisor,
%% for a fake one. This fake group leader is propagated to all of this
%% supervisor's children.
%%
%% This prevents any library that decides to write anything to
%% standard output from corrupting the messages sent through JSONRPC.
%% This problem is happening for example when calling `edoc:get_doc/2',
%% which can print warnings to standard output.
-spec restrict_stdio_access() -> ok.
restrict_stdio_access() ->
  ?LOG_INFO("Use group leader as io_device"),
  case application:get_env(els_core, io_device, standard_io) of
    standard_io ->
      application:set_env(els_core, io_device, erlang:group_leader());
    _ -> ok
  end,

  ?LOG_INFO("Replace group leader to avoid unwanted output to stdout"),
  Pid = erlang:spawn(fun noop_group_leader/0),
  erlang:group_leader(Pid, self()),
  ok.

%% @doc Simulate a group leader but do nothing
-spec noop_group_leader() -> no_return().
noop_group_leader() ->
  receive
    Message ->
      ?LOG_INFO("noop_group_leader got [message=~p]", [Message]),
      case Message of
        {io_request, From, ReplyAs, getopts} ->
          %% We need to pass the underlying io opts, otherwise shell_docs does
          %% not know which encoding to use. See #754
          From ! {io_reply, ReplyAs, io:getopts()};
        {io_request, From, ReplyAs, _} ->
          From ! {io_reply, ReplyAs, ok};
        _ ->
          ok
      end,
      noop_group_leader()
  end.
