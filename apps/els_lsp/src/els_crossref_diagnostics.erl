%%==============================================================================
%% Crossref diagnostics
%% Like xref, but using the internal indexing results
%%==============================================================================
-module(els_crossref_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([
    is_default/0,
    run/1,
    source/0
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").
%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
    false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    EnabledDiagnostics = els_diagnostics:enabled_diagnostics(),
    CompilerEnabled = lists:member(<<"compiler">>, EnabledDiagnostics),
    Start = erlang:monotonic_time(millisecond),
    Res =
        case els_utils:lookup_document(Uri) of
            {error, _Error} ->
                [];
            {ok, Document} ->
                POIs = els_dt_document:pois(Document, kinds()),
                Opts = #{
                    compiler_enabled => CompilerEnabled
                },
                {Diags, _Cache} =
                    lists:mapfoldl(
                        fun(#{id := Id} = POI, Cache) ->
                            case find_in_cache(Id, Cache) of
                                {ok, HasDef} ->
                                    {[make_diagnostic(HasDef, POI)], Cache};
                                error ->
                                    HasDef = has_definition(POI, Document, Opts),
                                    {
                                        make_diagnostic(HasDef, POI),
                                        update_cache(HasDef, Id, Cache)
                                    }
                            end
                        end,
                        #{},
                        POIs
                    ),
                lists:flatten(Diags)
        end,
    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,
    ?LOG_DEBUG("Crossref done for ~p [duration: ~p ms]", [els_uri:module(Uri), Duration]),
    Res.

-spec source() -> binary().
source() ->
    <<"CrossRef">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec update_cache(true | {missing, function | module}, els_poi:poi_id(), map()) -> map().
update_cache({missing, module}, {M, _F, _A}, Cache) ->
    %% Cache missing module to avoid repeated lookups
    Cache#{M => missing};
update_cache(HasDef, Id, Cache) ->
    Cache#{Id => HasDef}.

-spec find_in_cache(els_poi:poi_id(), map()) -> _.
find_in_cache({M, _F, _A}, Cache) when is_map_key(M, Cache) ->
    {ok, {missing, module}};
find_in_cache(Id, Cache) ->
    maps:find(Id, Cache).

-spec kinds() -> [els_poi:poi_kind()].
kinds() ->
    [
        application,
        implicit_fun,
        import_entry,
        export_entry,
        nifs_entry
    ].

-spec make_diagnostic(_, els_poi:poi()) -> [els_diagnostics:diagnostic()].
make_diagnostic({missing, Kind}, #{id := Id} = POI) ->
    Message = error_msg(Kind, Id),
    Severity = ?DIAGNOSTIC_ERROR,
    [
        els_diagnostics:make_diagnostic(
            els_protocol:range(range(Kind, POI)),
            Message,
            Severity,
            source()
        )
    ];
make_diagnostic(true, _) ->
    [].

-spec range(module | function | no_export, els_poi:poi()) -> els_poi:poi_range().
range(module, #{data := #{mod_range := Range}}) ->
    Range;
range(function, #{data := #{name_range := Range}}) ->
    Range;
range(no_export, #{data := #{name_range := Range}}) ->
    Range;
range(_, #{range := Range}) ->
    Range.

-spec error_msg(module | function | no_export, els_poi:poi_id()) -> binary().
error_msg(module, {M, _F, _A}) ->
    els_utils:to_binary(io_lib:format("Cannot find module ~p", [M]));
error_msg(function, Id) ->
    els_utils:to_binary(io_lib:format("Cannot find definition for function ~s", [id_str(Id)]));
error_msg(no_export, Id) ->
    els_utils:to_binary(io_lib:format("Cannot find export for function ~s", [id_str(Id)])).

-spec id_str(els_poi:poi_id()) -> string().
id_str(Id) ->
    case Id of
        {F, A} -> lists:flatten(io_lib:format("~p/~p", [F, A]));
        {M, F, A} -> lists:flatten(io_lib:format("~p:~p/~p", [M, F, A]))
    end.

-spec has_definition(els_poi:poi(), els_dt_document:item(), _) ->
    true | {missing, module | function | no_export}.
has_definition(#{data := #{imported := true}}, _Document, _Opts) ->
    %% Call to a bif
    true;
has_definition(#{id := {module_info, 0}}, _, _) ->
    true;
has_definition(#{id := {module_info, 1}}, _, _) ->
    true;
has_definition(#{data := #{mod_is_variable := true}}, _, _) ->
    true;
has_definition(#{data := #{fun_is_variable := true}}, _, _) ->
    true;
has_definition(#{id := {Module, module_info, Arity}}, _, _) when Arity =:= 0; Arity =:= 1 ->
    case els_dt_document_index:lookup(Module) of
        {ok, []} ->
            {missing, module};
        {ok, _} ->
            true
    end;
has_definition(#{id := {record_info, 2}}, _, _) ->
    true;
has_definition(#{id := {behaviour_info, 1}}, _, _) ->
    true;
has_definition(#{id := {lager, Level, Arity}}, _, _) ->
    lager_definition(Level, Arity);
has_definition(#{id := {lists, append, 1}}, _, _) ->
    %% lists:append/1 isn't indexed for some reason
    true;
has_definition(
    #{id := {F, A}} = POI,
    Document,
    #{
        %% Compiler already checks local function calls
        compiler_enabled := false
    }
) ->
    Uri = els_dt_document:uri(Document),
    MFA = {els_uri:module(Uri), F, A},
    case function_lookup(MFA) of
        true ->
            true;
        false ->
            case els_code_navigation:goto_definition(Uri, POI) of
                {ok, _Defs} ->
                    true;
                {error, _Error} ->
                    {missing, function}
            end
    end;
has_definition(#{id := {M, _F, _A} = MFA} = POI, _Document, _Opts) ->
    case els_utils:find_module(M) of
        {ok, Uri} ->
            case els_code_navigation:goto_definition(Uri, POI) of
                {ok, _Defs} ->
                    case function_lookup(MFA) of
                        true ->
                            true;
                        false ->
                            {missing, no_export}
                    end;
                {error, _Error} ->
                    {missing, function}
            end;
        {error, _} ->
            {missing, module}
    end;
has_definition(_POI, #{uri := _Uri}, _Opts) ->
    true.

-spec function_lookup(mfa()) -> boolean().
function_lookup(MFA) ->
    case els_db:lookup(els_dt_functions:name(), MFA) of
        {ok, []} ->
            false;
        {ok, _} ->
            true
    end.

-spec lager_definition(atom(), integer()) -> boolean().
lager_definition(Level, Arity) when Arity =:= 1 orelse Arity =:= 2 ->
    case lists:member(Level, lager_levels()) of
        true ->
            true;
        false ->
            {missing, function}
    end;
lager_definition(_, _) ->
    {missing, function}.

-spec lager_levels() -> [atom()].
lager_levels() ->
    [debug, debug_unsafe, info, notice, warning, error, critical, alert, emergency].
