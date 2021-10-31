-module(els_test).

-include_lib("els_core/include/els_core.hrl").
-include_lib("stdlib/include/assert.hrl").

-opaque session() :: #{uri := uri()}.
-type source() :: binary().
-type code() :: binary().
-type simplified_diagnostic() :: #{ code => code()
                                  , range => {pos(), pos()}
                                  }.
-export_type([ session/0 ]).

-export([ start_session/1
        , wait_for_diagnostics/2
        , assert_errors/2
        , assert_warnings/2
        , assert_hints/2
        , assert_contains/2
        ]).

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

-spec assert_errors([els_diagnostics:diagnostic()],
                    [els_diagnostics:diagnostic()]) -> ok.
assert_errors(Expected, Diagnostics) ->
  assert_diagnostics(Expected, Diagnostics, ?DIAGNOSTIC_ERROR).

-spec assert_warnings([els_diagnostics:diagnostic()],
                      [els_diagnostics:diagnostic()]) -> ok.
assert_warnings(Expected, Diagnostics) ->
  assert_diagnostics(Expected, Diagnostics, ?DIAGNOSTIC_WARNING).

-spec assert_hints([els_diagnostics:diagnostic()],
                   [els_diagnostics:diagnostic()]) -> ok.
assert_hints(Expected, Diagnostics) ->
  assert_diagnostics(Expected, Diagnostics, ?DIAGNOSTIC_HINT).

-spec assert_diagnostics([els_diagnostics:diagnostic()],
                         [els_diagnostics:diagnostic()],
                         els_diagnostics:severity()) -> ok.

-spec assert_contains(els_diagnostics:diagnostic(),
                      [els_diagnostics:diagnostic()]) -> ok.
assert_contains(Diagnostic, Diagnostics) ->
  Simplified = [simplify_diagnostic(D) || D  <- Diagnostics],
  ?assert(lists:member(Diagnostic, Simplified)).

assert_diagnostics(Expected, Diagnostics, Severity) ->
  Filtered = [D || #{severity := S} = D <- Diagnostics, S =:= Severity],
  Simplified = [simplify_diagnostic(D) || D  <- Filtered],
  ?assertEqual(Expected, Simplified, Filtered).

-spec simplify_diagnostic(els_diagnostics:diagnostic()) ->
        simplified_diagnostic().
simplify_diagnostic(Diagnostic) ->
  Range = simplify_range(maps:get(range, Diagnostic)),
  maps:put(range, Range,
           maps:remove(severity,
                       maps:remove(source, Diagnostic))).

-spec simplify_range(range()) -> {pos(), pos()}.
simplify_range(Range) ->
  #{ start := #{ character := CharacterStart
               , line := LineStart
               }
   , 'end' := #{ character := CharacterEnd
               , line := LineEnd
               }
   } = Range,
  {{LineStart, CharacterStart}, {LineEnd, CharacterEnd}}.
