%%==============================================================================
%% Erlang LS & Refactor Erl conversion
%%==============================================================================
-module(els_refactorerl_utils).

%%==============================================================================
%% API
%%==============================================================================
-export([ referl_node/0
        , query/1
        , notification/1
        , notification/2
        , make_diagnostics/2
        , source_name/0
        , add/1
        ]).
      
%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").
-define(MAX_RECURSION_DEPTH, 10).

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
referl_node() -> 
  case els_config:get(refactorerl) of
    #{"node" := {Node, validated}} ->
      {ok, Node};

    #{"node" := {Node, disconnected}} ->
      connect_node({retry, Node});

    #{"node" := {_Node, disabled}} ->
      {error, disabled};

    #{"node" := NodeStr} ->
      RT = els_config_runtime:get_name_type(),
      Node = els_utils:compose_node_name(NodeStr, RT),
      connect_node({validate, Node});

    notconfigured ->
      notification("RefactorErl is not configured!"),
      {error, disabled};

    _ ->
      {error, other}
  end.


%%@doc
%% Adds a module to the RefactorErl node. Using the UI router
%% Returns 'ok' if successfull
%% Returns 'error' if it fails after ?MAX_RECURSION_DEPTH number of tries
-spec add(uri()) -> atom().
add(Uri) -> 
  add(Uri, 0).

-spec add(uri(), number()) -> atom().
add(Uri, RecursionDepth) when RecursionDepth < ?MAX_RECURSION_DEPTH ->
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
          disable_node(Node),
          error;
        ok ->
          receive
            {ReqID, reply, {ok, _}} -> ok
          end;
        deny ->
          notification("Adding is deined, retry!"), % TODO: Take this out overtime stability proves itself
          timer:sleep(1000),
          add(Uri, RecursionDepth + 1)
      end;
    _ ->
      error
  end;

add(_Uri, RecursionDepth) when RecursionDepth >= ?MAX_RECURSION_DEPTH ->
  notification("Cannot add module to RefactorErl!", ?MESSAGE_TYPE_ERROR),
  error.

  
%%@doc
%% Runs a Query on the RefactorErl node, returns its result if successfull
%% If error happens, it just returns an empty list, as most of times the
%% result then is directly converted to POIs.
%% If the node timeouts or badrpc will come back, it disables the node and
%% stills returns an empty list
-spec query(string()) -> list().
query(Query) ->
  query(Query, 0).

-spec query(string(), number()) -> list().
query(Query, RecursionDepth) when RecursionDepth < ?MAX_RECURSION_DEPTH-> 
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
          disable_node(Node),
          []; % As most of times it is going to be processed right after.
        ok ->
          receive
            {ReqID, reply, Result} -> Result
          end;
        deny ->
          timer:sleep(1000),
          query(Query, RecursionDepth + 1)
      end
  end;

query(_Query, RecursionDepth) when RecursionDepth >= ?MAX_RECURSION_DEPTH ->
  notification("Cannot execute query on RefactorErl!", ?MESSAGE_TYPE_ERROR),
  [].


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


%%@doc
%% Creates a list of diagnostics form the RefactorErl results and a message
%% Message can be the same, as this is called per diagnsotic type.
%% The severity is only warning.
-spec make_diagnostics(any(), string()) -> [els_diagnostics:diagnostic()].
make_diagnostics([{{_Path, From, To}, Name} | Tail], DiagMsg) ->
  Range = #{ from => From, to => To },
  Id = refactorerl_poi,
  #{ data := PoiData, range := PoiRange} =
                        els_poi:new(Range, application, Id, Name), 
  RangeLS = els_protocol:range(PoiRange),
  Message = list_to_binary(DiagMsg ++ " " ++ PoiData),
  Severity = ?DIAGNOSTIC_WARNING,
  Source = source_name(),
  Diag = els_diagnostics:make_diagnostic(RangeLS, Message, Severity, Source), 
  [ Diag | make_diagnostics(Tail, DiagMsg) ];

make_diagnostics([], _) ->
  [];

make_diagnostics({ok, {result, [{result,[{group_by, {nopos, _}, list, L}]}]}}, 
                                                                    DiagMsg) ->
  make_diagnostics(L, DiagMsg).

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
%% Tries to connect to a node. 
%% When it status is validate, the node hasn't been checked yet, 
%% so it will reports the success, and failure as well,
%% 
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
%% Gets a request id from the RefactorErl node, and returns it 
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

%%@doc
%% Common soruce name for all RefactorErl based backend(s) 
-spec source_name() -> binary().
source_name() ->
  <<"RefactorErl">>.