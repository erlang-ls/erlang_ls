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
    Config = #{
        task => fun get_docs/2,
        entries => [{Uri, Line, Character}],
        title => <<"Hover">>,
        on_complete =>
            fun(HoverResp) ->
                els_server ! {result, HoverResp, self()},
                ok
            end
    },
    {ok, Pid} = els_background_job:new(Config),
    Pid.

-spec get_docs({uri(), integer(), integer()}, undefined) -> map() | null.
get_docs({Uri, Line, Character}, _) ->
    {ok, Doc} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:get_element_at_pos(Doc, Line + 1, Character + 1),
    do_get_docs(Uri, POIs).

-spec do_get_docs(uri(), [els_poi:poi()]) -> map() | null.
do_get_docs(_Uri, []) ->
    null;
do_get_docs(Uri, [POI | Rest]) ->
    case els_docs:docs(Uri, POI) of
        [] ->
            do_get_docs(Uri, Rest);
        Entries ->
            #{contents => els_markup_content:new(Entries)}
    end.
