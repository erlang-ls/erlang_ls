%%==============================================================================
%% Background Job Server
%%==============================================================================
-module(els_background_job).

%%==============================================================================
%% API
%%==============================================================================
-export([ start_link/1 ]).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type entry() :: any().
-type config() :: #{ task := fun((entry()) -> ok)
                   , entries := [any()]
                   , on_complete => fun()
                     %% TODO: Add label
                   }.
-type state() :: #{config := config()}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(config()) -> {ok, pid()}.
start_link(Config) ->
  gen_server:start_link(?MODULE, Config, []).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-spec init(config()) -> {ok, state()}.
init(Config) ->
  self() ! init,
  {ok, #{config => Config}}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {noreply, state()}.
handle_call(_Request, _From, State) ->
  {noreply, State}.

-spec handle_cast(any(), any()) ->
  {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), any()) ->
  {noreply, state()}.
handle_info(init, State) ->
  #{config := Config} = State,
  #{task := Task, entries := Entries} = Config,
  Step = step(Entries),
  F = fun(Entry, Progress) ->
          %% TODO: Handle failures, eg log, add test
          Task(Entry),
          erlang:display(Entry),
          %% TODO: Do not hard-code notification
          Msg = io_lib:format("Progress: ~p", [floor(Progress * Step)]),
          els_server:send_notification(<<"window/showMessage">>,
                                       #{ type => 3, %% TODO: Hard-coded
                                          message => list_to_binary(Msg)
                                        }),
          Progress + 1
      end,
  lager:info("Completing"),
  Res = lists:foldl(F, 1, Entries),
  case maps:is_key(on_complete, Config) of
    true ->
      OnCompleteFun = maps:get(on_complete, Config),
      OnCompleteFun();
    false ->
      ok
  end,
  lager:info("Completed"),
  erlang:display({res, Res}),
  {stop, normal, State};
handle_info(_Request, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec step([entry()]) -> pos_integer().
step([]) ->
  100;
step(Entries) ->
  100 / length(Entries).
