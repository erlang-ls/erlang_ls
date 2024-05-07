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
    TS = erlang:timestamp(),
    {ok, [Document]} = els_dt_document:lookup(Uri),
    %% Fetch all application POIs that are in the given range
    AppPOIs = els_dt_document:pois_in_range(Document, [application], Range),
    ArgHints = lists:flatmap(fun(POI) -> arg_hints(Uri, POI) end, AppPOIs),

    %% Fetch all function_clause POIs that are in the given range
    FunPOIs = els_dt_document:pois_in_range(Document, [function_clause], Range),
    %% Fetch all export entries
    ExportPOIs = els_dt_document:pois(Document, [export_entry]),
    FunHints = lists:flatmap(fun(POI) -> fun_hints(POI, ExportPOIs) end, FunPOIs),

    ?LOG_DEBUG(
        "Inlay hints took ~p ms",
        [timer:now_diff(erlang:timestamp(), TS) div 1000]
    ),
    ArgHints ++ FunHints.

%% @doc Output inlay hints for a function clause,
%%      indicate if a function is exported or not
-spec fun_hints(els_poi:poi(), [els_poi:poi()]) -> [inlay_hint()].
fun_hints(#{id := {F, A, _}, range := Range}, ExportPOIs) ->
    case lists:any(fun(#{id := Id}) -> Id == {F, A} end, ExportPOIs) of
        true ->
            [fun_exported_hint(Range)];
        false ->
            []
    end.

-spec fun_exported_hint(els_poi:poi_range()) -> inlay_hint().
fun_exported_hint(#{from := {FromL, FromC}}) ->
    #{
        position => #{line => FromL - 1, character => FromC - 1},
        label => unicode:characters_to_binary("exp"),
        paddingRight => true,
        kind => ?INLAY_HINT_KIND_TYPE
    }.

-spec arg_hints(uri(), els_poi:poi()) -> [inlay_hint()].
arg_hints(Uri, #{kind := application, data := #{args := CallArgs}} = POI) ->
    lists:flatmap(
        fun(#{index := N, range := ArgRange, name := Name}) ->
            case els_code_navigation:goto_definition(Uri, POI) of
                {ok, [{DefUri, DefPOI} | _]} ->
                    DefArgs = get_args(DefUri, DefPOI),
                    DefArgName = arg_name(N, DefArgs),
                    case should_show_arg_hint(Name, DefArgName) of
                        true ->
                            [arg_hint(ArgRange, DefArgName)];
                        false ->
                            []
                    end;
                {error, _} ->
                    []
            end
        end,
        CallArgs
    ).

-spec arg_hint(els_poi:poi_range(), string()) -> inlay_hint().
arg_hint(#{from := {FromL, FromC}}, ArgName) ->
    #{
        position => #{line => FromL - 1, character => FromC - 1},
        label => unicode:characters_to_binary(ArgName ++ ":"),
        paddingRight => true,
        kind => ?INLAY_HINT_KIND_PARAMETER
    }.

-spec should_show_arg_hint(
    string() | undefined,
    string() | undefined
) ->
    boolean().
should_show_arg_hint(Name, Name) ->
    false;
should_show_arg_hint(_Name, undefined) ->
    false;
should_show_arg_hint(undefined, _Name) ->
    true;
should_show_arg_hint(Name, DefArgName) ->
    strip_trailing_digits(Name) /= strip_trailing_digits(DefArgName).

-spec strip_trailing_digits(string()) -> string().
strip_trailing_digits(String) ->
    string:trim(String, trailing, "0123456789").

-spec wait_for_indexing_job(uri()) -> ok.
wait_for_indexing_job(Uri) ->
    %% Add delay to allowing indexing job to start
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

-spec arg_name(non_neg_integer(), els_arg:args()) -> string() | undefined.
arg_name(_N, []) ->
    undefined;
arg_name(N, Args) ->
    case lists:nth(N, Args) of
        #{name := "_" ++ Name} ->
            Name;
        #{name := Name} ->
            Name
    end.

-spec get_args(uri(), els_poi:poi()) -> els_arg:args().
get_args(Uri, #{
    id := {F, A},
    data := #{args := Args}
}) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    SpecPOIs = els_dt_document:pois(Document, [spec]),
    SpecMatches = [
        SpecArgs
     || #{id := Id, data := #{args := SpecArgs}} <- SpecPOIs,
        Id == {F, A},
        SpecArgs /= []
    ],
    case SpecMatches of
        [] ->
            Args;
        [SpecArgs | _] ->
            els_arg:merge_args(SpecArgs, Args)
    end.
