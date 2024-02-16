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
%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
    false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    case els_utils:lookup_document(Uri) of
        {error, _Error} ->
            [];
        {ok, Document} ->
            POIs = els_dt_document:pois(Document, [
                application,
                implicit_fun,
                import_entry,
                export_entry
            ]),
            [make_diagnostic(POI) || POI <- POIs, not has_definition(POI, Document)]
    end.

-spec source() -> binary().
source() ->
    <<"CrossRef">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec make_diagnostic(els_poi:poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{range := Range, id := Id}) ->
    Function =
        case Id of
            {F, A} -> lists:flatten(io_lib:format("~p/~p", [F, A]));
            {M, F, A} -> lists:flatten(io_lib:format("~p:~p/~p", [M, F, A]))
        end,
    Message = els_utils:to_binary(
        io_lib:format(
            "Cannot find definition for function ~s",
            [Function]
        )
    ),
    Severity = ?DIAGNOSTIC_ERROR,
    els_diagnostics:make_diagnostic(
        els_protocol:range(Range),
        Message,
        Severity,
        source()
    ).

-spec has_definition(els_poi:poi(), els_dt_document:item()) -> boolean().
has_definition(
    #{
        kind := application,
        id := {module_info, 0}
    },
    _
) ->
    true;
has_definition(
    #{
        kind := application,
        id := {module_info, 1}
    },
    _
) ->
    true;
has_definition(
    #{
        kind := application,
        data := #{mod_is_variable := true}
    },
    _
) ->
    true;
has_definition(
    #{
        kind := application,
        id := {Module, module_info, Arity}
    },
    _
) when Arity =:= 0; Arity =:= 1 ->
    {ok, []} =/= els_dt_document_index:lookup(Module);
has_definition(
    #{
        kind := application,
        id := {record_info, 2}
    },
    _
) ->
    true;
has_definition(
    #{
        kind := application,
        id := {behaviour_info, 1}
    },
    _
) ->
    true;
has_definition(
    #{
        kind := application,
        data := #{fun_is_variable := true}
    },
    _
) ->
    true;
has_definition(
    #{
        kind := application,
        id := {lager, Level, Arity}
    },
    _
) ->
    lager_definition(Level, Arity);
has_definition(POI, #{uri := Uri}) ->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, _Defs} ->
            true;
        {error, _Error} ->
            false
    end.

-spec lager_definition(atom(), integer()) -> boolean().
lager_definition(Level, Arity) when Arity =:= 1 orelse Arity =:= 2 ->
    lists:member(Level, lager_levels());
lager_definition(_, _) ->
    false.

-spec lager_levels() -> [atom()].
lager_levels() ->
    [debug, info, notice, warning, error, critical, alert, emergency].
