%%==============================================================================
%% Unused Includes diagnostics
%%==============================================================================
-module(els_unused_macros_diagnostics).

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
    true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    case filename:extension(Uri) of
        <<".erl">> ->
            case els_utils:lookup_document(Uri) of
                {error, _Error} ->
                    [];
                {ok, Document} ->
                    UnusedMacros = find_unused_macros(Document),
                    [make_diagnostic(POI) || POI <- UnusedMacros]
            end;
        _ ->
            []
    end.

-spec source() -> binary().
source() ->
    <<"UnusedMacros">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec find_unused_macros(els_dt_document:item()) -> [els_poi:poi()].
find_unused_macros(Document) ->
    Defines = els_dt_document:pois(Document, [define]),
    Macros = els_dt_document:pois(Document, [macro]),
    MacroIds = [Id || #{id := Id} <- Macros],
    [POI || #{id := Id} = POI <- Defines, not lists:member(Id, MacroIds)].

-spec make_diagnostic(els_poi:poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{id := POIId, range := POIRange}) ->
    Range = els_protocol:range(POIRange),
    MacroName =
        case POIId of
            {Id, Arity} ->
                els_utils:to_binary(
                    lists:flatten(io_lib:format("~s/~p", [Id, Arity]))
                );
            Id ->
                atom_to_binary(Id, utf8)
        end,
    Message = <<"Unused macro: ", MacroName/binary>>,
    Severity = ?DIAGNOSTIC_WARNING,
    Source = source(),
    els_diagnostics:make_diagnostic(Range, Message, Severity, Source).
