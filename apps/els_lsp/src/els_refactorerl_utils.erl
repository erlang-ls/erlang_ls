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
        , notification/1
        , notification/2
        ]).

%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================

%% @doc
%% Returns the RefactorErl node, if it cannot, it will return error and the cause.
%% It returns the node given in config, if it is alive.
%% First it runs the validation functions, the result of validation will be 
%% notified to the user. 
%% If the node once was validated there will be no display messages.
%%
%% The configuration can store the node and its status.
%%  - Either a simple NodeString, which needs to be checked and configure
%%  - {Node, Status} where boths are atoms
%%      Possible statuses: validated, disconnected, disabled
%%      'disabled',       when it won't try to reconnect
%%      'disconnected',   when it will try to reconnect
%%  - notconfigured, the node is not configured in the config file
%%  
%%
%% Node can be:
%%  - NodeStr
%%  - {Status, Node} where both are atoms.
%%  - Status can be:
%%    - validated:    node is running
%%    - disconnected: node is not running
%%  - disabled: RefactorErl is turned off for this session. T
%%              his can happen after an unsuccessfull query attempt.
-spec referl_node() -> {ok, atom()}
                     | {error, disconnected}
                     | {error, disabled}
                     | {error, other}.
referl_node() ->
  case els_config:get(refactorerl) of
    #{"node" := {Node, validated}} ->
      {ok, Node};

    #{"node" := {Node, disconnected}} ->
      try_connect_node({retry, Node});

    #{"node" := {_, disabled}} ->
      {error, disabled};
    
    #{"node" := NodeStr} ->
      RT = els_config_runtime:get_name_type(),
      Node = els_utils:compose_node_name(NodeStr, RT),
      try_connect_node({validate, Node});

    disabled ->
      notification("RefactorErl is not configured!"),
      {error, disabled};

    _ ->
      {error, other}
  end.


%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec disable_node(atom()) -> atom().
%%@doc
%% Disables the node given in paramter, also notifes the user.
disable_node(Node) ->
  Msg = "RefactorErl is disconnected!
  Reload ELS after you fixed theRefactorErl node!",
  notification(Msg, ?MESSAGE_TYPE_ERROR),

  els_config:set(refactorerl, #{"node" => {Node, disabled}}),
  {error, disabled}.


  %%@doc
  %% Runs a Query on the RefactorErl node, returns its result if successfull
  %% If error happens, it just returns an empty list, as most of times the
  %% result then is directly converted to POIs.
  %% If the node timeouts or badrpc will come back, it disables the node
-spec query(string()) -> list() | {error, disabled}.
query(Query) ->
  case referl_node() of
    {error, _} ->
      [];
    {ok, Node} ->
      P = [{positions, linecol}, {output, msg}],
      Response = rpc:call(Node, refusr_sq, run, [P, [], Query], maxtimeout()),
      case Response of
        {badrpc, _} ->
          disable_node(Node); % Returns disabled
        error ->
          [];
        _ ->
          Response
      end
  end.

-spec convert_to_poi(any()) -> poi().
%%@doc
%% Convert a RefactorErl result to a POI, for the LS system
%% The RefactorErl format is how the refusr_sq:run\3 returns
convert_to_poi(ReferlResult) ->
case ReferlResult of
    [{_, _, _, DataList}] ->
      convert_to_poi(DataList);
    [{{_, {FromLine, FromCol}, {ToLine, ToCol}}, Name} | Tail] ->
      Range = #{ from => {FromLine, FromCol}, to => {ToLine, ToCol} },
      Id = refactorerl_poi, %"{module(), 'atom()', 'arity()''}",
      Poi = els_poi:new(Range, application, Id, Name),
      [ Poi | convert_to_poi(Tail) ];
    _ ->
        [] %TODO Robi notify
end.


%%@doc
%% Calls the RefactorErl node, to check if it's alive using ri:ls()
-spec check_node(atom()) -> {error, timeout}
                          | {error, badrpc}
                          | {error, other}
                          | atom().
check_node(Node) ->
  Response = rpc:call(Node, ri, ls, [], 10000),
  case Response of
    {{ok, _}, {error, _}} ->
      Node;
    ok ->
      Node;
    {badrpc, timeout} ->
      {error, timeout};
    {badrpc, _} ->
      {error, badrpc};
    _ ->
      {error, other}
  end.

%%@doc
%% Tries to connect to a node
%% When statues is validate, it reports the success, and failure as well, when retry, it won't report.
-spec try_connect_node({validate | retry, atom()}) -> {error, disconnected}
                                                     | atom().
try_connect_node({Status, Node}) ->
  case {Status, check_node(Node)} of
    {validate, {error, _}} ->
      notification("RefactorErl is not connected!", ?MESSAGE_TYPE_INFO),
      els_config:set(refactorerl, #{"node" => {Node, disconnected}}),
      {error, disconnected};
    {retry, {error, _}} ->
      els_config:set(refactorerl, #{"node" => {Node, disconnected}}),
      {error, disconnected};
    {_, Node} ->
      notification("RefactorErl is connected!", ?MESSAGE_TYPE_INFO),
      els_config:set(refactorerl, #{"node" => {Node, validated}}),
      {ok, Node}
  end.

%%@doc
%% Util for popping up notifications
-spec notification(string(), number()) -> atom().
notification(Msg, Severity) ->
  Param = #{ type => Severity,
             message => list_to_binary(Msg) },
  els_server:send_notification(<<"window/showMessage">>, Param).

-spec notification(string()) -> atom().
notification(Msg) ->
  notification(Msg, ?MESSAGE_TYPE_INFO).


%%==============================================================================
%% Values
%%==============================================================================

-spec maxtimeout() -> number().
maxtimeout() ->
    10000.
