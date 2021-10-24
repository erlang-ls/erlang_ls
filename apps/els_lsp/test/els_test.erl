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

-spec assert_errors([els_diagnostics:diagnostic()], any()) -> ok.
assert_errors(Expected, Diagnostics) ->
  Filtered = [D || #{severity := ?DIAGNOSTIC_ERROR} = D <- Diagnostics],
  Simplified = [simplify_diagnostic(D) || D  <- Filtered],
  ?assertMatch(Expected, Simplified, Filtered).

-spec assert_warnings([els_diagnostics:diagnostic()], any()) -> ok.
assert_warnings(Expected, Diagnostics) ->
  Filtered = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Simplified = [simplify_diagnostic(D) || D  <- Filtered],
  ?assertMatch(Expected, Simplified, Filtered).

-spec simplify_diagnostic(els_diagnostics:diagnostic()) ->
        simplified_diagnostic().
simplify_diagnostic(Diagnostic) ->
  #{ code := Code
   , range := #{ start := #{ character := CharacterStart
                           , line := LineStart
                           }
               , 'end' := #{ character := CharacterEnd
                           , line := LineEnd
                           }
               } } = Diagnostic,
  #{ code => Code
   , range => { {LineStart, CharacterStart}
              , {LineEnd, CharacterEnd}
              }
   }.
