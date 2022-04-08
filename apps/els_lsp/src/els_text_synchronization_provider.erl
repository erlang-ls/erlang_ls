-module(els_text_synchronization_provider).

-behaviour(els_provider).
-export([ handle_request/2
        , init/0
        , is_enabled/0
        , options/0
        ]).

-include("els_lsp.hrl").

-type state() :: #{in_progress => [progress_entry()]}.
-type progress_entry() :: {uri(), job()}.
-type job() :: pid().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{ openClose => true
   , change    => els_text_synchronization:sync_mode()
   , save      => #{includeText => false}
   }.

-spec init() -> state().
init() ->
  #{ in_progress => [] }.

-spec handle_request(any(), state()) -> {job(), state()}.
handle_request({did_open, Params}, State) ->
  ok = els_text_synchronization:did_open(Params),
  {ok, State};
handle_request({did_change, Params}, State) ->
  #{in_progress := InProgress0} = State,
  #{<<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  InProgress = cancel_jobs(Uri, InProgress0),
  Job = els_text_synchronization:did_change(Params),
  {ok, State#{in_progress => [{Uri, Job}|InProgress]}};
handle_request({did_save, Params}, State) ->
  ok = els_text_synchronization:did_save(Params),
  {ok, State};
handle_request({did_close, Params}, State) ->
  ok = els_text_synchronization:did_close(Params),
  {ok, State};
handle_request({did_change_watched_files, Params}, State) ->
  ok = els_text_synchronization:did_change_watched_files(Params),
  {ok, State}.

-spec cancel_jobs(uri(), [progress_entry()]) -> [progress_entry()].
cancel_jobs(Uri, InProgress) ->
  Fun = fun({U, Job}) ->
            case U =:= Uri of
              true ->
                els_background_job:stop(Job),
                false;
              false ->
                true
            end
        end,
  lists:filtermap(Fun, InProgress).
