%%==============================================================================
%% Unsecure Calls by Referl
%%==============================================================================
-module(els_refactorerl_diagnostics).

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
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").
-define(MAX_RECURSION_DEPTH, 10).

%%==============================================================================
%% Callback Functions
%%==============================================================================


%%=======================================
%%=======================================

%% {id, pre, after}
%% 
%% 
%% -type diagnostic_id() :: binary().

-type refactorerl_diagnostic_id() :: {atom(), [char()], [char()]}.
-type refactorerl_query() :: [char()].

-spec refactorerl_diagnostics() -> [refactorerl_diagnostic_id()].
refactorerl_diagnostics() ->
  [ {unused_macros,  "mods[name=", "].funs.unsecure_calls"} 
  , {unsecure_calls, "mods[name=", "].macros[not .references]"}
  ].


-spec make_query(refactorerl_diagnostic_id(), module()) -> refactorerl_query().
make_query({_, Before, After}, Module) ->
  ModuleStr = atom_to_list(Module),
  Before ++ ModuleStr ++ After.


-spec run_query(module(),refactorerl_diagnostic_id()) -> ([els_diagnostics:diagnostic()]).
run_query(Module, DiagnosticId) ->
  ReferlResult = els_refactorerl_utils:query(make_query(DiagnosticId, Module)),
  Pois = els_refactorerl_utils:convert_to_poi(ReferlResult),
  [make_diagnostic(Poi) || Poi <- Pois].


%%=======================================
%%=======================================
-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri)->
  run(Uri, 0).


-spec run(uri(), number()) -> [els_diagnostics:diagnostic()]. 
run(Uri, RecursionDepth) when RecursionDepth < ?MAX_RECURSION_DEPTH ->
  els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_ERROR, message => <<"HERH!">> }),
  case filename:extension(Uri) of
    <<".erl">> -> 
      case els_refactorerl_utils:referl_node() of
        disabled ->
          [];
        Node -> 
          case rpc:call(Node, ri, add, [binary_to_list(els_uri:path(Uri))], els_refactorerl_utils:maxtimeout()) of
            busy ->
              timer:sleep(1000),
              run(Uri, RecursionDepth + 1);
            disabled ->
              [];
            _ ->
              Module = list_to_atom(filename:rootname(filename:basename(binary_to_list(els_uri:path(Uri))))),
              lists:concat([run_query(Module, DiagnosticId) || DiagnosticId <- refactorerl_diagnostics()])
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
  <<"RefactorErl Diagnostics">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{ data := PoiData, range := PoiRange}) -> %#{id := POIId, range := POIRange} .   
    Range = els_protocol:range(PoiRange),
    IssueName = list_to_binary(PoiData), % TODO Robi: the ID and the arity should be encoded to the ID
    Message = <<"Security Issue: ", IssueName/binary>>,
    Severity = ?DIAGNOSTIC_WARNING,
    Source = source(),
    els_diagnostics:make_diagnostic(Range, Message, Severity, Source).