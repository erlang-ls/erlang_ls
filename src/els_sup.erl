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
%% supervisors callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => rest_for_one
              , intensity => 5
              , period    => 60
              },
  {ok, Transport} = application:get_env(erlang_ls, transport),
  %% Restrict access to stdio when using that transport
  restrict_stdio_access(Transport),
  ChildSpecs = [ #{ id       => els_config
                  , start    => {els_config, start_link, []}
                  , shutdown => brutal_kill
                  }
               , #{ id    => els_providers_sup
                  , start => {els_providers_sup, start_link, []}
                  , type  => supervisor
                  }
               , #{ id    => els_background_job_sup
                  , start => {els_background_job_sup, start_link, []}
                  , type  => supervisor
                  }
               , #{ id    => els_distribution_sup
                  , start => {els_distribution_sup, start_link, []}
                  , type  => supervisor
                  }
               , #{ id       => els_server
                  , start    => {els_server, start_link, [Transport]}
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
-spec restrict_stdio_access(els_stdio | els_tcp) -> ok.
restrict_stdio_access(els_stdio) ->
  lager:info("Use group leader as io_device"),
  case application:get_env(erlang_ls, io_device, standard_io) of
    standard_io ->
      application:set_env(erlang_ls, io_device, erlang:group_leader());
    _ -> ok
  end,

  lager:info("Replace group leader to avoid unwanted output to stdout"),
  Pid = erlang:spawn(fun noop_group_leader/0),
  erlang:group_leader(Pid, self()),

  ok;
restrict_stdio_access(_) ->
  ok.

%% @doc Simulate a group leader but do nothing
-spec noop_group_leader() -> no_return().
noop_group_leader() ->
  receive
    Message ->
      lager:info("noop_group_leader got [message=~p]", [Message]),
      case Message of
        {io_request, From, ReplyAs, _} ->
          From ! {io_reply, ReplyAs, ok};
        _ ->
          ok
      end,
      noop_group_leader()
  end.
