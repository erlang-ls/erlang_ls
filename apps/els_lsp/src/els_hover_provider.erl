%%==============================================================================
%% Hover Provider
%%==============================================================================
-module(els_hover_provider).

-behaviour(els_provider).

-export([ handle_info/2
        , handle_request/2
        , is_enabled/0
        , init/0
        , cancel_request/2
        ]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: #{in_progress => [progress_entry()]}.
-type progress_entry() :: {uri(), job()}.
-type job() :: pid().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec init() -> state().
init() ->
  #{ in_progress => []}.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({hover, Params}, State) ->
  #{in_progress := InProgress} = State,
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
   ?LOG_DEBUG("Starting hover job ""[uri=~p, line=~p, character=~p]"
             , [Uri, Line, Character]
             ),
   Job = run_hover_job(Uri, Line, Character),
   {Job, State#{in_progress => [{Uri, Job}|InProgress]}}.


-spec handle_info(any(), state()) -> state().
handle_info({result, HoverResp, Job}, State) ->
  ?LOG_DEBUG("Received hover result [job=~p]", [Job]),
  #{ in_progress := InProgress } = State,
  els_server:send_response(Job, HoverResp),
  State#{ in_progress => lists:keydelete(Job, 2, InProgress) }.

-spec cancel_request(job(), state()) -> state().
cancel_request(Job, State) ->
  ?LOG_DEBUG("Cancelling hover [job=~p]", [Job]),
  els_background_job:stop(Job),
  #{ in_progress := InProgress } = State,
  State#{ in_progress => lists:keydelete(Job, 2, InProgress) }.


%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec run_hover_job(uri(), line(), column()) -> pid().
run_hover_job(Uri, Line, Character) ->
  {ok, Doc} = els_utils:lookup_document(Uri),
  POIs = els_dt_document:get_element_at_pos(Doc, Line + 1, Character + 1),
  Config = #{ task => fun get_docs/2
            , entries => [{Uri, POIs}]
            , title => <<"Hover">>
            , on_complete =>
                fun(HoverResp) ->
                    ?SERVER ! {result, HoverResp, self()},
                    ok
                end
            },
  {ok, Pid} = els_background_job:new(Config),
  Pid.

-spec get_docs({uri(), [poi()]}, undefined) -> map() | null.
get_docs({Uri, POIs}, _) ->
  do_get_docs(Uri, POIs).


-spec do_get_docs(uri(), [poi()]) -> map() | null.
do_get_docs(_Uri, []) ->
  null;
do_get_docs(Uri, [POI|Rest]) ->
  case els_docs:docs(Uri, POI) of
    [] ->
      do_get_docs(Uri, Rest);
    Entries ->
      #{contents => els_markup_content:new(Entries)}
  end.
