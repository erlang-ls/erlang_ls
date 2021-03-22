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
-type progress_entry() :: {uri(), job()}.
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

-spec handle_request(any(), state()) -> {job(), state()}.
handle_request({document_codelens, Params}, State) ->
  #{in_progress := InProgress} = State,
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  ?LOG_DEBUG("Starting lenses job [uri=~p]", [Uri]),
  Job = run_lenses_job(Uri),
  {Job, State#{in_progress => [{Uri, Job}|InProgress]}}.

-spec handle_info(any(), state()) -> state().
handle_info({result, Lenses, Job}, State) ->
  ?LOG_DEBUG("Received lenses result [job=~p]", [Job]),
  #{ in_progress := InProgress } = State,
  els_server:send_response(Job, Lenses),
  State#{ in_progress => lists:keydelete(Job, 2, InProgress) }.

-spec cancel_request(job(), state()) -> state().
cancel_request(Job, State) ->
  ?LOG_DEBUG("Cancelling lenses [job=~p]", [Job]),
  els_background_job:stop(Job),
  #{ in_progress := InProgress } = State,
  State#{ in_progress => lists:keydelete(Job, 2, InProgress) }.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec run_lenses_job(uri()) -> pid().
run_lenses_job(Uri) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  Config = #{ task =>
                fun(Doc, _) ->
                    lists:flatten(
                      [els_code_lens:lenses(Id, Doc) ||
                        Id <- els_code_lens:enabled_lenses()])
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
  Pid.
