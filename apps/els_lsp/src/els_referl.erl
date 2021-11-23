%%==============================================================================
%% Eralang LS & Refactor Erl conversion
%%==============================================================================
-module(els_referl).

%%==============================================================================
%% API
%%==============================================================================
-export([ convertToPoi/1
        , referl_node/0
        , maxtimeout/0
        ]).

%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").

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


%%==============================================================================
%% Values
%%==============================================================================

-spec maxtimeout() -> number().
maxtimeout() -> 
    3000.