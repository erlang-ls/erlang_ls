-module(els_diagnostics_provider).

-behaviour(els_provider).

-export([ handle_info/2
        , handle_request/2
        , init/0
        , is_enabled/0
        , options/0
        ]).

-export([ notify/2
        , publish/2
        ]).

-include("erlang_ls.hrl").

-type state() :: #{in_progress => [progress_entry()]}.
-type progress_entry() :: #{ uri := uri()
                           , pending := [job()]
                           , diagnostics := [els_diagnostics:diagnostic()]
                           }.
-type job() :: pid().

-define(SERVER, ?MODULE).

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{}.

-spec init() -> state().
init() ->
  #{ in_progress => [] }.

%% LSP 3.15 introduce versioning for diagnostics. Until all clients
%% support it, we need to keep track of old diagnostics and re-publish
%% them every time we get a new chunk.
-spec handle_info(any(), state()) -> state().
handle_info({diagnostics, Diagnostics, Job}, State) ->
  lager:debug("Received diagnostics [job=~p]", [Job]),
  #{ in_progress := InProgress } = State,
  { #{ pending := Jobs
     , diagnostics := OldDiagnostics
     , uri := Uri
     }
  , Rest
  } = find_entry(Job, InProgress),
  NewDiagnostics = Diagnostics ++ OldDiagnostics,
  ?MODULE:publish(Uri, NewDiagnostics),
  case lists:delete(Job, Jobs) of
    [] ->
      State#{in_progress => Rest};
    Remaining ->
      State#{in_progress => [#{ pending => Remaining
                              , diagnostics => NewDiagnostics
                              , uri => Uri
                              }|Rest]}
  end;
handle_info(_Request, State) ->
  State.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({run_diagnostics, Params}, State) ->
  #{in_progress := InProgress} = State,
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  lager:debug("Starting diagnostics jobs [uri=~p]", [Uri]),
  Jobs = els_diagnostics:run_diagnostics(Uri),
  Entry = #{uri => Uri, pending => Jobs, diagnostics => []},
  {noresponse, State#{in_progress => [Entry|InProgress]}}.

%%==============================================================================
%% API
%%==============================================================================
-spec notify([els_diagnostics:diagnostic()], pid()) -> ok.
notify(Diagnostics, Job) ->
  ?SERVER ! {diagnostics, Diagnostics, Job},
  ok.

-spec publish(uri(), [els_diagnostics:diagnostic()]) -> ok.
publish(Uri, Diagnostics) ->
  Method = <<"textDocument/publishDiagnostics">>,
  Params = #{ uri         => Uri
            , diagnostics => Diagnostics
            },
  els_server:send_notification(Method, Params).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec find_entry(job(), [progress_entry()]) ->
        {progress_entry(), [progress_entry()]}.
find_entry(Job, InProgress) ->
  find_entry(Job, InProgress, []).

-spec find_entry(job(), [progress_entry()], [progress_entry()]) ->
        {progress_entry(), [progress_entry()]}.
find_entry(Job, [#{pending := Pending} = Entry|Rest], Acc) ->
  case lists:member(Job, Pending) of
    true ->
      {Entry, Rest ++ Acc};
    false ->
      find_entry(Job, Rest, [Entry|Acc])
  end.
