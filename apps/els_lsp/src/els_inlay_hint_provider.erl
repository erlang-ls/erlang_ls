-module(els_inlay_hint_provider).

-behaviour(els_provider).

-export([
    handle_request/1,
    options/0
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("els_core/include/els_core.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {async, uri(), pid()}.
handle_request({inlay_hint, Params}) ->
    #{
        <<"range">> := Range,
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    ?LOG_DEBUG(
        "Inlay hint provider was called with params: ~p",
        [Params]
    ),
    PoiRange = els_range:to_poi_range(Range),
    {ok, Job} = run_inlay_hints_job(Uri, PoiRange),
    {async, Uri, Job}.

-spec options() -> boolean().
options() ->
    els_config:get(inlay_hints_enabled).

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec run_inlay_hints_job(uri(), els_poi:poi_range()) -> {ok, pid()}.
run_inlay_hints_job(Uri, Range) ->
    Config = #{
        task => fun get_inlay_hints/2,
        entries => [{Uri, Range}],
        title => <<"Inlay Hints">>,
        on_complete => fun els_server:register_result/1
    },
    els_background_job:new(Config).

-spec get_inlay_hints({uri(), els_poi:poi_range()}, any()) ->
    [inlay_hint()].
get_inlay_hints({Uri, Range}, _) ->
    %% Wait for indexing job to finish, so that we have the updated document
    wait_for_indexing_job(Uri),
    %% Read the document to get the latest version
    {ok, [Document]} = els_dt_document:lookup(Uri),
    %% Fetch all application POIs that are in the given range
    AppPOIs = els_dt_document:pois_in_range(Document, [application], Range),
    [
        arg_hint(ArgRange, ArgName)
     || #{data := #{args := CallArgs}} = POI <- AppPOIs,
        #{index := N, name := Name, range := ArgRange} <- CallArgs,
        #{data := #{args := DefArgs}} <- [definition(Uri, POI)],
        ArgName <- [arg_name(N, DefArgs)],
        should_show_arg_hint(Name, ArgName)
    ].

-spec arg_hint(els_poi:poi_range(), string()) -> inlay_hint().
arg_hint(#{from := {FromL, FromC}}, ArgName) ->
    #{
        position => #{line => FromL - 1, character => FromC - 1},
        label => unicode:characters_to_binary(ArgName ++ ":"),
        paddingRight => true,
        kind => ?INLAY_HINT_KIND_PARAMETER
    }.

-spec should_show_arg_hint(string() | undefined, string() | undefined) ->
    boolean().
should_show_arg_hint(Name, Name) ->
    false;
should_show_arg_hint(_Name, undefined) ->
    false;
should_show_arg_hint(_Name, _DefArgName) ->
    true.

-spec wait_for_indexing_job(uri()) -> ok.
wait_for_indexing_job(Uri) ->
    %% Add delay to allowing indexing job to finish
    timer:sleep(10),
    JobTitles = els_background_job:list_titles(),
    case lists:member(<<"Indexing ", Uri/binary>>, JobTitles) of
        false ->
            %% No indexing job is running, we're ready!
            ok;
        true ->
            %% Indexing job is still running, retry until it finishes
            wait_for_indexing_job(Uri)
    end.

-spec arg_name(non_neg_integer(), els_parser:args()) -> string() | undefined.
arg_name(N, Args) ->
    #{name := Name0} = lists:nth(N, Args),
    case Name0 of
        "_" ++ Name ->
            Name;
        Name ->
            Name
    end.

-spec definition(uri(), els_poi:poi()) -> els_poi:poi() | error.
definition(Uri, POI) ->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, [{_Uri, DefPOI} | _]} ->
            DefPOI;
        Err ->
            ?LOG_INFO("Error: ~p ~p", [Err, POI]),
            error
    end.
