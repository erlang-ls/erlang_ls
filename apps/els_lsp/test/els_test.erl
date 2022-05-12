-module(els_test).

-include_lib("els_core/include/els_core.hrl").
-include_lib("stdlib/include/assert.hrl").

-opaque session() :: #{uri := uri()}.
-type source() :: binary().
-type code() :: binary().
-type simplified_diagnostic() :: #{
    code => code(),
    range => {pos(), pos()}
}.
-export_type([session/0]).

-export([
    run_diagnostics_test/5,
    start_session/1,
    wait_for_diagnostics/2,
    assert_errors/3,
    assert_warnings/3,
    assert_hints/3,
    assert_contains/2,
    compiler_returns_column_numbers/0
]).

-spec run_diagnostics_test(
    string(),
    source(),
    [els_diagnostics:diagnostic()],
    [els_diagnostics:diagnostic()],
    [els_diagnostics:diagnostic()]
) -> ok.
run_diagnostics_test(Path, Source, Errors, Warnings, Hints) ->
    {ok, Session} = start_session(Path),
    Diagnostics = wait_for_diagnostics(Session, Source),
    assert_errors(Source, Errors, Diagnostics),
    assert_warnings(Source, Warnings, Diagnostics),
    assert_hints(Source, Hints, Diagnostics).

-spec start_session(string()) -> {ok, session()}.
start_session(Path0) ->
    PrivDir = code:priv_dir(els_lsp),
    Path = filename:join([els_utils:to_binary(PrivDir), Path0]),
    Uri = els_uri:uri(Path),
    {ok, #{uri => Uri}}.

-spec wait_for_diagnostics(session(), source()) ->
    [els_diagnostics:diagnostic()].
wait_for_diagnostics(#{uri := Uri}, Source) ->
    els_mock_diagnostics:subscribe(),
    ok = els_client:did_save(Uri),
    Diagnostics = els_mock_diagnostics:wait_until_complete(),
    [D || #{source := S} = D <- Diagnostics, S =:= Source].

-spec assert_errors(
    source(),
    [els_diagnostics:diagnostic()],
    [els_diagnostics:diagnostic()]
) -> ok.
assert_errors(Source, Expected, Diagnostics) ->
    assert_diagnostics(Source, Expected, Diagnostics, ?DIAGNOSTIC_ERROR).

-spec assert_warnings(
    source(),
    [els_diagnostics:diagnostic()],
    [els_diagnostics:diagnostic()]
) -> ok.
assert_warnings(Source, Expected, Diagnostics) ->
    assert_diagnostics(Source, Expected, Diagnostics, ?DIAGNOSTIC_WARNING).

-spec assert_hints(
    source(),
    [els_diagnostics:diagnostic()],
    [els_diagnostics:diagnostic()]
) -> ok.
assert_hints(Source, Expected, Diagnostics) ->
    assert_diagnostics(Source, Expected, Diagnostics, ?DIAGNOSTIC_HINT).

-spec assert_contains(
    els_diagnostics:diagnostic(),
    [els_diagnostics:diagnostic()]
) -> ok.
assert_contains(Diagnostic, Diagnostics) ->
    Simplified = [simplify_diagnostic(D) || D <- Diagnostics],
    ?assert(lists:member(Diagnostic, Simplified)).

-spec assert_diagnostics(
    source(),
    [els_diagnostics:diagnostic()],
    [els_diagnostics:diagnostic()],
    els_diagnostics:severity()
) -> ok.
assert_diagnostics(Source, Expected, Diagnostics, Severity) ->
    Filtered = [D || #{severity := S} = D <- Diagnostics, S =:= Severity],
    Simplified = [simplify_diagnostic(D) || D <- Filtered],
    FixedExpected = [maybe_fix_range(Source, D) || D <- Expected],
    ?assertEqual(
        lists:sort(FixedExpected),
        lists:sort(Simplified),
        Filtered
    ).

-spec simplify_diagnostic(els_diagnostics:diagnostic()) ->
    simplified_diagnostic().
simplify_diagnostic(Diagnostic) ->
    Range = simplify_range(maps:get(range, Diagnostic)),
    maps:put(
        range,
        Range,
        maps:remove(
            severity,
            maps:remove(source, Diagnostic)
        )
    ).

-spec simplify_range(range()) -> {pos(), pos()}.
simplify_range(Range) ->
    #{
        start := #{
            character := CharacterStart,
            line := LineStart
        },
        'end' := #{
            character := CharacterEnd,
            line := LineEnd
        }
    } = Range,
    {{LineStart, CharacterStart}, {LineEnd, CharacterEnd}}.

maybe_fix_range(<<"Compiler">>, Diagnostic) ->
    case compiler_returns_column_numbers() of
        false ->
            fix_range(Diagnostic);
        true ->
            Diagnostic
    end;
maybe_fix_range(_Source, Diagnostic) ->
    Diagnostic.

fix_range(#{code := <<"L0000">>} = Diagnostic) ->
    Diagnostic;
fix_range(Diagnostic) ->
    #{range := Range} = Diagnostic,
    {{StartLine, StartCol}, {EndLine, EndCol}} = Range,
    case StartCol =/= 0 orelse EndCol =/= 0 of
        true ->
            Diagnostic#{range => {{StartLine, 0}, {EndLine + 1, 0}}};
        false ->
            Diagnostic
    end.

compiler_returns_column_numbers() ->
    %% If epp:open/5 is exported we know that columns are not
    %% returned by the compiler warnings and errors.
    %% Should find a better heuristic for this.
    not erlang:function_exported(epp, open, 5).
