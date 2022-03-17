-module(els_code_lens_provider).

-behaviour(els_provider).
-export([ handle_info/2
        , handle_request/2
        , init/0
        , is_enabled/0
        , options/0
        , cancel_request/2
        ]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-type state() :: #{in_progress => [progress_entry()]}.
-type progress_entry() :: #{ uri := uri()
                           , pending := [job()]
                           , lenses := [els_code_lens:lens()]
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
  #{ resolveProvider => false }.

-spec init() -> state().
init() ->
  #{ in_progress => [] }.

%% TODO: Do lenses use versioning?
-spec handle_request(any(), state()) -> {job(), state()}.
handle_request({document_codelens, Params}, State) ->
  #{in_progress := InProgress} = State,
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  ?LOG_DEBUG("Starting lenses job [uri=~p]", [Uri]),
  Jobs = run_lenses(Uri),
  Entry = #{uri => Uri, pending => Jobs, lenses => []},
  {noresponse, State#{in_progress => [Entry|InProgress]}}.

-spec handle_info(any(), state()) -> state().
handle_info({result, Lenses, Job}, State) ->
  ?LOG_DEBUG("Received lenses result [job=~p]", [Job]),
  #{ in_progress := InProgress } = State,
  { #{ pending := Jobs
     , lenses := OldLenses
     , uri := Uri
     }
  , Rest
  } = find_entry(Job, InProgress),
  NewLenses = Lenses ++ OldLenses,
  case lists:delete(Job, Jobs) of
    [] ->
      els_server:send_response(Job, NewLenses),
      State#{ in_progress => Rest };
    Remaining ->
      State#{ in_progress => [#{ pending => Remaining
                               , lenses => NewLenses
                               , uri => Uri
                               }|Rest]}
  end.

-spec cancel_request(job(), state()) -> state().
cancel_request(Job, State) ->
  ?LOG_DEBUG("Cancelling lenses [job=~p]", [Job]),
  els_background_job:stop(Job),
  #{ in_progress := InProgress } = State,
  State#{ in_progress => lists:keydelete(Job, 2, InProgress) }.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec run_lenses(uri()) -> [pid()].
run_lenses(Uri) ->
  lists:flatten(
    [ run_lenses(Uri, Id) || Id <- els_code_lens:enabled_lenses()]).

run_lenses(Uri, Id) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  Config = #{ task =>
                fun(Doc, _) ->
                    els_code_lens:lenses(Id, Doc)
                end
            , entries => [Document]
            , title => <<"Lenses">>
            , on_complete =>
                fun(Lenses) ->
                    ?SERVER ! {result, Lenses, self()},
                    ok
                end
            },
  {ok, Pid} = els_background_job:new(Config),
  [Pid].

%% TODO: Refactor (duplicated in diagnostics provider)
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
