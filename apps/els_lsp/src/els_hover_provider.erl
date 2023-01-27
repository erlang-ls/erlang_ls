%%==============================================================================
%% Hover Provider
%%==============================================================================
-module(els_hover_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {async, uri(), pid()}.
handle_request({hover, Params}) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    ?LOG_DEBUG(
        "Starting hover job " "[uri=~p, line=~p, character=~p]",
        [Uri, Line, Character]
    ),
    Job = run_hover_job(Uri, Line, Character),
    {async, Uri, Job}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec run_hover_job(uri(), line(), column()) -> pid().
run_hover_job(Uri, Line, Character) ->
    {ok, Doc} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:get_element_at_pos(Doc, Line + 1, Character + 1),
    Config = #{
        task => fun get_docs/2,
        entries => [{Uri, POIs}],
        title => <<"Hover">>,
        on_complete =>
            fun(HoverResp) ->
                els_server ! {result, HoverResp, self()},
                ok
            end
    },
    {ok, Pid} = els_background_job:new(Config),
    Pid.

-spec get_docs({uri(), [els_poi:poi()]}, undefined) -> map() | null.
get_docs({Uri, POIs}, _) ->
    Pid = self(),
    spawn(fun() -> do_get_docs(Uri, POIs, Pid) end),
    receive
        HoverResp -> HoverResp
    end.

-spec do_get_docs(uri(), [els_poi:poi()], pid()) -> map() | null.
do_get_docs(_Uri, [], Pid) ->
    Pid ! null;
do_get_docs(Uri, [POI | Rest], Pid) ->
    case els_docs:docs(Uri, POI) of
        [] ->
            do_get_docs(Uri, Rest, Pid);
        Entries ->
            Pid ! #{contents => els_markup_content:new(Entries)}
    end.
