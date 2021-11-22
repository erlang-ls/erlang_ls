%%==============================================================================
%% Unsecure Calls by Referl
%%==============================================================================
-module(els_referl_unsecure_calls_diagnostics).

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
-define(TIME_OUT, 3000).

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
      case referl_node() of
        disabled ->
          [];
        Node -> 
          case rpc:call(Node, ri, add, [binary_to_list(els_uri:path(Uri))], ?TIME_OUT) of
            {badrpc, _} ->
              els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_ERROR, message => <<"Refactor Erl node is down {badrpc}!">> }),
              [];
            error ->
              timer:sleep(1000),
              run(Uri, RecursionDepth + 1);
            _ ->
              ModuleName = filename:rootname(filename:basename(binary_to_list(els_uri:path(Uri)))),
              ReferlResult = rpc:call(Node, refusr_sq, run, [[{positions, linecol}, {output, msg}], [], "mods[name=" ++ ModuleName ++ "].funs.unsecure_calls"]),
              Pois = convertToPoi(ReferlResult),
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
  <<"Unsecure Calls (Referl)">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec convertToPoi(any()) -> poi(). 
convertToPoi(ReferlResult) ->
case ReferlResult of
    [{_, _, _, DataList}] -> % [{Option, Tuple, Atom, DataList}] = Out
      convertToPoi(DataList);
    [{{_, {FromLine, FromCol}, {ToLine, ToCol}}, Name} | Tail] -> %  [ {{Path, StartPos, EndPos}, MacroName} | Tail] = DataList
      Range = #{ from => {FromLine, FromCol}, to => {ToLine, ToCol} },
      Id = referl_atom_unsec, %"{module(), 'atom()', 'arity()''}",
      Poi = els_poi:new(Range, application, Id, Name), % Additional Data param can be added
      [ Poi | convertToPoi(Tail) ];
    _ ->
        [] %TODO Robi notify
end.

-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{ data := PoiData, range := PoiRange}) -> %#{id := POIId, range := POIRange} .   
    Range = els_protocol:range(PoiRange),
    IssueName = list_to_binary(PoiData), % TODO Robi: the ID and the arity should be encoded to the ID
    Message = <<"Security Issue: ", IssueName/binary>>,
    Severity = ?DIAGNOSTIC_WARNING,
    Source = source(),
    els_diagnostics:make_diagnostic(Range, Message, Severity, Source).

  
-spec referl_node() -> atom(). % If called with no args, then try to get it from config
referl_node() ->
  case els_config:get(refactorerl) of
    {checked, #{"node" := Node}} ->
      Node;
    {config, #{"node" := NodeStr}} ->
      Node = els_utils:compose_node_name(NodeStr, els_config_runtime:get_name_type()),
      case referl_node(Node) of
        error ->
          els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_INFO, message => <<"RefactorErl is not connected! (error)">> }),
          disabled;
        badrpc -> % TODO: Robi Try other nodes. (default nodes like: referl@host)
          els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_INFO, message => <<"RefactorErl is not connected! (badrpc/timeout)!">> }),
          disabled;
        Node ->
          els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_INFO, message => <<"RefactorErl is connected!">> }),
          Node
        end;
    {config, disabled} ->
      disabled;
    {error, _} ->
      disabled
  end.

-spec referl_node(atom()) -> atom().
referl_node(Node) ->
  Response = rpc:call(Node, ri, ls, [], 10000), % TODO: ROBI van e valami referl ping?
  case Response of
    {{ok, _}, {error, _}} ->
      Node;
    ok->
      Node;
    {badrpc, _} -> %TODO: Robi Separate timeout issues
      badrpc;
    _ ->
      error
  end.