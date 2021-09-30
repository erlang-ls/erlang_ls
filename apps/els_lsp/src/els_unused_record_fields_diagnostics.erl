%%==============================================================================
%% Unused Includes diagnostics
%%==============================================================================
-module(els_unused_record_fields_diagnostics).

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
          UnusedRecordFields = find_unused_record_fields(Document),
          [make_diagnostic(POI) || POI <- UnusedRecordFields ]
      end;
    _ ->
      []
  end.

-spec source() -> binary().
source() ->
  <<"UnusedRecordFields">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec find_unused_record_fields(els_dt_document:item()) -> [poi()].
find_unused_record_fields(Document) ->
  Definitions = els_dt_document:pois(Document, [record_def_field]),
  Usages = els_dt_document:pois(Document, [record_field]),
  UsagesIds = lists:usort([Id || #{id := Id} <- Usages]),
  [POI || #{id := Id} = POI <- Definitions, not lists:member(Id, UsagesIds)].

-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{id := {RecName, RecField}, range := POIRange}) ->
  Range = els_protocol:range(POIRange),
  FullName = els_utils:to_binary(io_lib:format("#~p.~p", [RecName, RecField])),
  Message = <<"Unused record field: ", FullName/binary>>,
  Severity = ?DIAGNOSTIC_WARNING,
  Source = source(),
  els_diagnostics:make_diagnostic(Range, Message, Severity, Source).
