%%==============================================================================
%% AtomTypo diagnostics
%% Catch common atom typos
%%==============================================================================
-module(els_atom_typo_diagnostics).

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
    case els_utils:lookup_document(Uri) of
        {error, _Error} ->
            [];
        {ok, Document} ->
            Atoms = [<<"false">>, <<"true">>, <<"undefined">>, <<"error">>],
            POIs = els_dt_document:pois(Document, [atom]),
            [
                make_diagnostic(POI, Atom)
             || #{id := Id} = POI <- POIs,
                Atom <- Atoms,
                atom_to_binary(Id, utf8) =/= Atom,
                els_utils:jaro_distance(atom_to_binary(Id, utf8), Atom) > 0.9
            ]
    end.

-spec source() -> binary().
source() ->
    <<"AtomTypo">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec make_diagnostic(els_poi:poi(), binary()) -> els_diagnostics:diagnostic().
make_diagnostic(#{range := Range}, Atom) ->
    Message = els_utils:to_binary(
        io_lib:format(
            "Atom typo? Did you mean: ~s",
            [Atom]
        )
    ),
    Severity = ?DIAGNOSTIC_WARNING,
    els_diagnostics:make_diagnostic(
        els_protocol:range(Range),
        Message,
        Severity,
        source()
    ).
