-module(els_provider).

%% API
-export([ handle_request/2
        , start_link/0
        , available_providers/0
        , cancel_request/1
        , cancel_request_by_uri/1
        ]).

-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

-callback handle_request(request(), any()) -> {async, uri(), pid()} |
                                              {response, any()} |
                                              {diagnostics, uri(), [pid()]} |
                                              noresponse.
-callback handle_info(any(), any()) -> any().
-optional_callbacks([handle_info/2]).

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
                  | els_text_synchronization_provider.
-type request()  :: {atom() | binary(), map()}.
-type state() :: #{ in_progress := [progress_entry()]
                  , in_progress_diagnostics := [diagnostic_entry()]
                  , open_buffers := [buffer()]
                  }.
-type buffer() :: uri().
-type progress_entry() :: {uri(), job()}.
-type diagnostic_entry() :: #{ uri := uri()
                             , pending := [job()]
                             , diagnostics := [els_diagnostics:diagnostic()]
                             }.
-type job() :: pid().
%% TODO: Redefining uri() due to a type conflict with request()
-type uri() :: binary().
-export_type([ config/0
             , provider/0
             , request/0
             , state/0
             ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% External functions
%%==============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, unused, []).

-spec handle_request(provider(), request()) -> any().
handle_request(Provider, Request) ->
  gen_server:call(?SERVER, {handle_request, Provider, Request}, infinity).

-spec cancel_request(pid()) -> any().
cancel_request(Job) ->
  gen_server:cast(?SERVER, {cancel_request, Job}).

-spec cancel_request_by_uri(uri()) -> any().
cancel_request_by_uri(Uri) ->
  gen_server:cast(?SERVER, {cancel_request_by_uri, Uri}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(unused) -> {ok, state()}.
init(unused) ->
  %% Ensure the terminate function is called on shutdown, allowing the
  %% job to clean up.
  process_flag(trap_exit, true),
  {ok, #{ in_progress => []
        , in_progress_diagnostics => []
        , open_buffers => []
        }}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {reply, any(), state()}.
handle_call({handle_request, Provider, Request}, _From, State) ->
  #{in_progress := InProgress, in_progress_diagnostics := InProgressDiagnostics}
    = State,
  case Provider:handle_request(Request, State) of
    {async, Uri, Job} ->
      {reply, {async, Job}, State#{in_progress => [{Uri, Job}|InProgress]}};
    {response, Response} ->
      {reply, {response, Response}, State};
    {diagnostics, Uri, Jobs} ->
      Entry = #{uri => Uri, pending => Jobs, diagnostics => []},
      NewState =
        State#{in_progress_diagnostics => [Entry|InProgressDiagnostics]},
      {reply, noresponse, NewState};
    noresponse ->
      {reply, noresponse, State}
  end.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({cancel_request, Job}, State) ->
  ?LOG_DEBUG("Cancelling request [job=~p]", [Job]),
  els_background_job:stop(Job),
  #{ in_progress := InProgress } = State,
  NewState = State#{ in_progress => lists:keydelete(Job, 2, InProgress) },
  {noreply, NewState};
handle_cast({cancel_request_by_uri, Uri}, State) ->
  #{ in_progress := InProgress0 } = State,
  Fun = fun({U, Job}) ->
            case U =:= Uri of
              true ->
                els_background_job:stop(Job),
                false;
              false ->
                true
            end
        end,
  InProgress = lists:filtermap(Fun, InProgress0),
  ?LOG_DEBUG("Cancelling requests by Uri [uri=~p]", [Uri]),
  NewState = State#{in_progress => InProgress},
  {noreply, NewState}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({result, Result, Job}, State) ->
  ?LOG_DEBUG("Received result [job=~p]", [Job]),
  #{in_progress := InProgress} = State,
  els_server:send_response(Job, Result),
  NewState = State#{in_progress => lists:keydelete(Job, 2, InProgress)},
  {noreply, NewState};
%% LSP 3.15 introduce versioning for diagnostics. Until all clients
%% support it, we need to keep track of old diagnostics and re-publish
%% them every time we get a new chunk.
handle_info({diagnostics, Diagnostics, Job}, State) ->
  #{in_progress_diagnostics := InProgress} = State,
  ?LOG_DEBUG("Received diagnostics [job=~p]", [Job]),
    case find_entry(Job, InProgress) of
      {ok, { #{ pending := Jobs
              , diagnostics := OldDiagnostics
              , uri := Uri
              }
           , Rest
           }} ->
        NewDiagnostics = Diagnostics ++ OldDiagnostics,
        els_diagnostics_provider:publish(Uri, NewDiagnostics),
        NewState = case lists:delete(Job, Jobs) of
                     [] ->
                       State#{in_progress_diagnostics => Rest};
                     Remaining ->
                       State#{in_progress_diagnostics =>
                                [#{ pending => Remaining
                                  , diagnostics => NewDiagnostics
                                  , uri => Uri
                                  }|Rest]}
                   end,
        {noreply, NewState};
      {error, not_found} ->
        {noreply, State}
    end;
handle_info(_Request, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #{in_progress := InProgress}) ->
  [els_background_job:stop(Job) || {_Uri, Job} <- InProgress],
  ok.

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
  , els_call_hierarchy_provider
  , els_text_synchronization_provider
  ].

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec find_entry(job(), [diagnostic_entry()]) ->
        {ok, {diagnostic_entry(), [diagnostic_entry()]}} |
        {error, not_found}.
find_entry(Job, InProgress) ->
  find_entry(Job, InProgress, []).

-spec find_entry(job(), [diagnostic_entry()], [diagnostic_entry()]) ->
        {ok, {diagnostic_entry(), [diagnostic_entry()]}} |
        {error, not_found}.
find_entry(_Job, [], []) ->
  {error, not_found};
find_entry(Job, [#{pending := Pending} = Entry|Rest], Acc) ->
  case lists:member(Job, Pending) of
    true ->
      {ok, {Entry, Rest ++ Acc}};
    false ->
      find_entry(Job, Rest, [Entry|Acc])
  end.
