%%==============================================================================
%% Eralang LS & Refactor Erl conversion
%%==============================================================================
-module(els_refactorerl_utils).

%%==============================================================================
%% API
%%==============================================================================
-export([ convert_to_poi/1
        , referl_node/0
        , maxtimeout/0
        , query/1
        ]).

%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
%% If called with no args try to get the node from config. Use the referl_node/1 to validate.
%% If the node once was validated there will be no display messages.
%% 
%% Node can be:
%%  - NodeStr
%%  - {Status, Node} where both are atoms. 
%%  - Status can be:
%%    - validated:    node is running
%%    - disconnected: node is not running
%%  - disabled: RefactorErl is turned off for this session. This can happen after an unsuccessfull query attempt.
-spec referl_node() -> atom() | {error, disconnected} | {error, disabled} | {error, other}.
referl_node() -> 
  case els_config:get(refactorerl) of
    #{"node" := {Node, validated}} ->
      Node;
    #{"node" := {Node, disconnected}} ->
      try_connect_node({retry, Node});
    #{"node" := disabled} ->
      {error, disabled};
    #{"node" := NodeStr} ->
      Node = els_utils:compose_node_name(NodeStr, els_config_runtime:get_name_type()),
      try_connect_node({validate, Node});
    _ -> 
      {error, other}
  end.

  
%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec disable_node() -> atom().
disable_node() ->
  els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_ERROR, message => <<"RefactorErl is disconnected! Reload ELS after you fixed theRefactorErl node!">> }),
  els_config:set(refactorerl, #{"node" => disabled}),
  {error, disabled}.

-spec query(string()) -> list() | {error, disabled}.
query(Query) -> 
  %% rpc:call(referl@fikoMac, refusr_sq, run, [[{positions, linecol}, {output, msg}], [], "mods.fun"], 3000)
  case referl_node() of
    {error, _} ->
      [];
    Node ->
      Response = rpc:call(Node, refusr_sq, run, [[{positions, linecol}, {output, msg}], [], Query], maxtimeout()),
      case Response of
        {badrpc, _} ->
          disable_node(); % Returns disabled
        error ->
          busy; % RefactorErl node is probably busy. 
        _ ->
          Response
      end
  end.

-spec convert_to_poi(any()) -> poi(). 
convert_to_poi(ReferlResult) ->
case ReferlResult of
    [{_, _, _, DataList}] -> % [{Option, Tuple, Atom, DataList}] = Out
      convert_to_poi(DataList);
    [{{_, {FromLine, FromCol}, {ToLine, ToCol}}, Name} | Tail] -> %  [ {{Path, StartPos, EndPos}, MacroName} | Tail] = DataList
      Range = #{ from => {FromLine, FromCol}, to => {ToLine, ToCol} },
      Id = referl_atom_unsec, %"{module(), 'atom()', 'arity()''}",
      Poi = els_poi:new(Range, application, Id, Name), % Additional Data param can be added
      [ Poi | convert_to_poi(Tail) ];
    _ ->
        [] %TODO Robi notify
end.
  
-spec check_node(atom()) -> {error, timeout} | {error, badrpc} | {error, other} | atom().
check_node(Node) ->
  Response = rpc:call(Node, ri, ls, [], 10000), % Calls the RefactorErl node, to check if it's alive
  case Response of
    {{ok, _}, {error, _}} ->
      Node;
    ok->
      Node;
    {badrpc, timeout} ->
      {error, timeout};
    {badrpc, _} ->
      {error, badrpc};
    _ ->
      {error, other}
  end.

-spec try_connect_node({validate | retry, atom()}) -> {error, disconnected} | atom() .
try_connect_node({Status, Node}) ->
  case {Status, check_node(Node)} of
    {validate, {error, _}} ->
      els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_INFO, message => <<"RefactorErl is not connected!">> }),
      els_config:set(refactorerl, #{"node" => {Node, disconnected}}),
      {error, disconnected};
    {retry, {error, _}} ->
      els_config:set(refactorerl, #{"node" => {Node, disconnected}}),
      {error, disconnected};
    {_ ,Node} ->
      els_server:send_notification(<<"window/showMessage">>, #{ type => ?MESSAGE_TYPE_INFO, message => <<"RefactorErl is connected!">> }),
      els_config:set(refactorerl, #{"node" => {Node, validated}}),
      Node
  end.


%%==============================================================================
%% Values
%%==============================================================================

-spec maxtimeout() -> number().
maxtimeout() -> 
    3000.
