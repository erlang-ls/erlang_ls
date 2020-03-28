%%==============================================================================
%% Xref diagnostics
%%==============================================================================
-module(els_xref_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  {ok, [Document]} = els_dt_document:lookup(Uri),
  POIs = els_dt_document:pois(Document, [ application
                                        , implicit_fun
                                        , import_entry
                                        , export_entry
                                        ]),
  [make_diagnostic(POI) || POI <- POIs, not has_definition(POI, Document)].

-spec source() -> binary().
source() ->
  <<"XRef">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{range := Range}) ->
  Message = <<"Cannot find definition for this function">>,
  Severity = ?DIAGNOSTIC_ERROR,
  els_diagnostics:make_diagnostic( els_protocol:range(Range)
                                 , Message
                                 , Severity
                                 , source()).

-spec has_definition(poi(), els_dt_document:item()) -> boolean().
has_definition(POI, #{uri := Uri}) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, _Uri, _POI} ->
      true;
    {error, _Error} ->
      false
  end.
