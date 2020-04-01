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
  false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case els_dt_document:lookup(Uri) of
    {ok, []} ->
      [];
    {ok, [Document|_]} ->
      POIs = els_dt_document:pois(Document, [ application
                                            , implicit_fun
                                            , import_entry
                                            , export_entry
                                            ]),
      [make_diagnostic(POI) || POI <- POIs, not has_definition(POI, Document)]
  end.

-spec source() -> binary().
source() ->
  <<"XRef">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{range := Range, id := Id}) ->
  Function = case Id of
               {F, A} -> lists:flatten(io_lib:format("~p/~p", [F, A]));
               {M, F, A} -> lists:flatten(io_lib:format("~p:~p/~p", [M, F, A]))
             end,
  Message = els_utils:to_binary(
              io_lib:format( "Cannot find definition for function ~s"
                           , [Function])),
  Severity = ?DIAGNOSTIC_ERROR,
  els_diagnostics:make_diagnostic( els_protocol:range(Range)
                                 , Message
                                 , Severity
                                 , source()).

-spec has_definition(poi(), els_dt_document:item()) -> boolean().
has_definition(#{ kind := application
                , id := {erlang, module_info, 0} }, _) -> true;
has_definition(#{ kind := application
                , id := {erlang, module_info, 1} }, _) -> true;
has_definition(#{ kind := application
                , id := {record_info, 2} }, _) -> true;
has_definition(#{ kind := application
                , id := {behaviour_info, 1} }, _) -> true;
has_definition(POI, #{uri := Uri}) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, _Uri, _POI} ->
      true;
    {error, _Error} ->
      false
  end.
