%%==============================================================================
%% Erlang LS & Refactor Erl conversion
%%==============================================================================
-module(els_refactorerl_utils).

%%==============================================================================
%% API
%%==============================================================================
-export([ referl_node/0
        , maxtimeout/0
        , query/1
        , notification/1
        , notification/2
        , make_diagnostics/2
        , source_name/0
        , add/1
        ]). % TODO exports???

%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================

%% @doc
%% Returns the RefactorErl node, if it can't, it returns error and its cause.
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
referl_node() -> % TODO Might be better to use envs in the future
  case els_config:get(refactorerl) of
    #{"node" := {Node, validated}} ->
      {ok, Node};

    #{"node" := {Node, disconnected}} ->
      connect_node({retry, Node});

    #{"node" := {_, disabled}} ->
      {error, disabled};

    #{"node" := NodeStr} ->
      RT = els_config_runtime:get_name_type(),
      Node = els_utils:compose_node_name(NodeStr, RT),
      connect_node({validate, Node});

    disabled ->
      notification("RefactorErl is not configured!"),
      {error, disabled};

    _ ->
      {error, other} % TODO: maybe check for 'notconfigured'
  end.

%%@doc
%% Adds a module to the RefactorErl node.
-spec add(any()) -> atom().
add(Uri) ->
  case els_refactorerl_utils:referl_node() of
    {ok, Node} ->
      Path = [binary_to_list(els_uri:path(Uri))],
      ReqID = request_id(),
      Resp = rpc:call(  Node
                      , reflib_ui_router
                      , request
                      , [self(), ReqID, {add_dir, Path}] ),
      case Resp of
        {badrpc, _} ->
          disable_node(Node); % Returns disabled
        ok ->
          receive
            {ReqID, reply, {ok, _}} -> ok
          end;
        deny ->
          notification("Query is denied!"),
          []
      end;
    _ ->
      error
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
query(Query) -> % TODO: give a second tought to the case
  case referl_node() of 
    {error, _} ->
      [];
    {ok, Node} ->
      DisplayOpt = [{positions, linecol}, {output, msg}],
      ReqID = request_id(),
      Opts = [  self()
              , ReqID
              , {transform, semantic_query 
                , [{ask_missing, false}
                , {display_opt, DisplayOpt}
                , {start_opt, []}
                , {querystr, Query}]}
             ],
      Resp = rpc:call(Node, reflib_ui_router, request,  Opts),
      case Resp of
        {badrpc, _} ->
          disable_node(Node); % Returns disabled
        ok ->
          receive
            {ReqID, reply, Result} -> Result
          end;
        deny ->
          notification("Query is denied!"),
          []
      end
  end.


%TODO spec
% TODO doku
% 

%%@doc
%% Creates a diagnostic form the Poi data and a Message
%% The severity is only warning.
%-spec make_diagnostic(poi(), [char()]) -> els_diagnostics:diagnostic().
-spec make_diagnostics(any(), string()) -> any().
make_diagnostics([{{_, From, To}, Name} | Tail], DiagMsg) ->
  Range = #{ from => From, to => To },
  Id = refactorerl_poi,
  #{ data := PoiData, range := PoiRange} = els_poi:new(Range, application, Id, Name), % TODO: How to make this line shorter???
  RangeLS = els_protocol:range(PoiRange),
  Message = list_to_binary(DiagMsg ++ " " ++ PoiData),
  Severity = ?DIAGNOSTIC_WARNING,
  Source = source_name(),
  Diagnostic = els_diagnostics:make_diagnostic(RangeLS, Message, Severity, Source), % TODO: How to make this line shorter???
  [ Diagnostic | make_diagnostics(Tail, DiagMsg) ];

make_diagnostics([], _) ->
  [];

make_diagnostics({ok, {result, [{result,[{group_by, {nopos, _}, list, L}]}]}}, DiagMsg) -> % TODO: How to make this line shorter???
  make_diagnostics(L, DiagMsg).


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
%% When statues is validate, it reports the success, and failure as well,
%% when retry, it won't report.
-spec connect_node({validate | retry, atom()}) -> {error, disconnected}
                                                     | atom().
connect_node({Status, Node}) ->
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


-spec request_id() -> nodedown | {reqid | string()}.
request_id() ->
  case referl_node() of
    {error, _} ->
      nodedown;
    {ok, Node} ->
      rpc:call(Node, reflib_ui_router, getid, [])
  end.

      


%%==============================================================================
%% Values
%%==============================================================================

-spec maxtimeout() -> number().
maxtimeout() ->
    10000.

%TODOcode organisation
 
-spec source_name() -> binary().
source_name() ->
  <<"RefactorErl">>.