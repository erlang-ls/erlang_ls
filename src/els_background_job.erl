%%==============================================================================
%% API and Implementation for Background Jobs
%%==============================================================================
-module(els_background_job).

%%==============================================================================
%% API
%%==============================================================================
-export([ new/1
        , list/0
        , stop/1
        , stop_all/0
        ]).

-export([ start_link/1
        ]).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type entry() :: any().
-type config() :: #{ task := fun((entry()) -> ok)
                   , entries := [any()]
                   , on_complete => fun()
                   , on_error => fun()
                   , title := binary()
                   }.
-type state() :: #{ config := config()
                  , progress_enabled := boolean()
                  , token := els_progress:token()
                  , current := pos_integer()
                  , step := pos_integer()
                  , total := non_neg_integer()
                  }.

%%==============================================================================
%% API
%%==============================================================================

%% @doc Create a new background job
-spec new(config()) -> {ok, pid()}.
new(Config) ->
  supervisor:start_child(els_background_job_sup, [Config]).

%% @doc Return the list of running background jobs
-spec list() -> [pid()].
list() ->
  [Pid || {_Id, Pid, _Type, _Modules}
            <- supervisor:which_children(els_background_job_sup)].

%% @doc Terminate a background job
-spec stop(pid()) -> ok.
stop(Pid) ->
  supervisor:terminate_child(els_background_job_sup, Pid).

%% @doc Terminate all background jobs
-spec stop_all() -> ok.
stop_all() ->
  [ok = supervisor:terminate_child(els_background_job_sup, Pid) ||
    Pid <- list()],
  ok.

%% @doc Start the server responsible for a background job
%%
%% To be used by the supervisor
-spec start_link(config()) -> {ok, pid()}.
start_link(Config) ->
  gen_server:start_link(?MODULE, Config, []).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-spec init(config()) -> {ok, state()}.
init(#{entries := Entries, title := Title} = Config) ->
  %% Ensure the terminate function is called on shutdown, allowing the
  %% job to clean up.
  process_flag(trap_exit, true),
  ProgressEnabled = els_work_done_progress:is_supported(),
  Total = length(Entries),
  Step = step(Total),
  Token = els_work_done_progress:send_create_request(),
  notify_begin(Token, Title, Total, ProgressEnabled),
  self() ! exec,
  {ok, #{ config => Config
        , progress_enabled => ProgressEnabled
        , token => Token
        , current => 0
        , step => Step
        , total => Total
        }}.

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
handle_info(exec, State) ->
  #{ config := #{ entries := Entries, task := Task} = Config
   , progress_enabled := ProgressEnabled
   , token := Token
   , current := Current
   , step := Step
   , total := Total
   } = State,
  case Entries of
    [] ->
      notify_end(Token, Total, ProgressEnabled),
      {stop, normal, State};
    [Entry|Rest] ->
      Task(Entry),
      notify_report(Token, Current, Step, Total, ProgressEnabled),
      self() ! exec,
      {noreply, State#{ config => Config#{ entries => Rest }
                      , current => Current + 1}}
  end;
handle_info(_Request, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(normal, #{config := Config}) ->
  lager:info("Background job completed. [pid=~p]", [self()]),
  OnComplete = maps:get(on_complete, Config, noop()),
  OnComplete(),
  ok;
terminate(Reason, #{config := Config}) ->
  lager:warning( "Background job aborted. [reason=~p] [pid=~p]"
               , [Reason, self()]),
  OnError = maps:get(on_error, Config, noop()),
  OnError(),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec step(pos_integer()) -> pos_integer().
step(0) -> 0;
step(N) -> 100 / N.

-spec progress_msg(non_neg_integer(), pos_integer()) -> binary().
progress_msg(Current, Total) ->
  list_to_binary(io_lib:format("~p / ~p", [Current, Total])).

-spec noop() -> fun().
noop() -> fun() -> ok end.

-spec notify_begin(els_progress:token(), binary(), pos_integer(), boolean()) ->
        ok.
notify_begin(Token, Title, Total, true) ->
  BeginMsg = progress_msg(0, Total),
  Begin = els_work_done_progress:value_begin(Title, BeginMsg, 0),
  els_progress:send_notification(Token, Begin);
notify_begin(_Token, _Title, _Total, false) ->
  ok.

-spec notify_report( els_progress:token(), pos_integer(), pos_integer()
                   , pos_integer(), boolean()) -> ok.
notify_report(Token, Current, Step, Total, true) ->
  Percentage = floor(Current * Step),
  ReportMsg = progress_msg(Current, Total),
  Report = els_work_done_progress:value_report(ReportMsg, Percentage),
  els_progress:send_notification(Token, Report);
notify_report(_Token, _Current, _Step, _Total, false) ->
  ok.

-spec notify_end(els_progress:token(), pos_integer(), boolean()) -> ok.
notify_end(Token, Total, true) ->
  EndMsg = progress_msg(Total, Total),
  End = els_work_done_progress:value_end(EndMsg),
  els_progress:send_notification(Token, End);
notify_end(_Token, _Total, false) ->
  ok.
