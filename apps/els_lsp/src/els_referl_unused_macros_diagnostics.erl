%%==============================================================================
%% Unused Macros diagnostics by Referl
%%==============================================================================
-module(els_referl_unused_macros_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).
-behaviour(els_referl).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").
-define(MAX_RECURSION_DEPTH, 10).


%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri)->
  run(Uri, 0).

-spec run(uri(), number()) -> [els_diagnostics:diagnostic()]. 
run(Uri, RecursionDepth) when RecursionDepth < ?MAX_RECURSION_DEPTH ->
  case filename:extension(Uri) of
    <<".erl">> -> 
      case els_referl:referl_node() of
        disabled ->
          [];
        Node -> 
          case rpc:call(Node, ri, add, [binary_to_list(els_uri:path(Uri))], els_referl:time_out()) of
            {badrpc, _} ->
              els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_ERROR, message => <<"Refactor Erl node is down {badrpc}!">> }),
              [];
            error ->
              timer:sleep(1000),
              run(Uri, RecursionDepth + 1);
            _ ->
              ModuleName = filename:rootname(filename:basename(binary_to_list(els_uri:path(Uri)))),
              ReferlResult = rpc:call(Node, refusr_sq, run, [[{positions, linecol}, {output, msg}], [], "mods[name=" ++ ModuleName ++ "].macros[not .references]"], els_referl:time_out()), % TODO: Robi sq run második paramba @ utén
              Pois = els_referl:convertToPoi(ReferlResult),
              [make_diagnostic(Poi) || Poi <- Pois]
          end
        end;
    _ ->
      []
  end;

run(_, RecursionDepth) when RecursionDepth >= ?MAX_RECURSION_DEPTH ->
  els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_ERROR, message => <<"Cannot add module to RefactorErl!">> }),
  [].

-spec source() -> binary().
source() ->
  <<"Unused Macros (Referl)">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{ data := PoiData, range := PoiRange}) -> %#{id := POIId, range := POIRange} .   
    Range = els_protocol:range(PoiRange),
    MacroName = list_to_binary(PoiData), % TODO Robi: the ID and the arity should be encoded to the ID
    Message = <<"Unused macro: ", MacroName/binary>>,
    Severity = ?DIAGNOSTIC_WARNING,
    Source = source(),
    els_diagnostics:make_diagnostic(Range, Message, Severity, Source).

