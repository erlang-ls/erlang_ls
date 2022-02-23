%%==============================================================================
%% Erlang LS & Refactor Erl conversion
%%==============================================================================
-module(els_refactorerl_utils).

%%==============================================================================
%% API
%%==============================================================================
-export([ referl_node/0
        %, query/1
        , notification/1
        , notification/2
        %, make_diagnostics/2
        , run_diagnostics/2
        , source_name/0
        , add/1
        ]).

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
-spec add(uri()) -> error | ok.
add(Uri) ->
  case els_refactorerl_utils:referl_node() of
    {ok, Node} ->
      Path = [binary_to_list(els_uri:path(Uri))],
      rpc:call(Node, referl_els, add, [Path]); %% returns error | ok
    _ ->
      error
  end.



-spec run_diagnostics(any(), any()) -> any(). %TODO: typing
run_diagnostics(DiagnosticAliases, Module) ->
  case els_refactorerl_utils:referl_node() of
    {ok, Node} ->
      rpc:call(Node, referl_els, run_diagnostics, [DiagnosticAliases, Module]); %% returns error | ok
    _ -> % In this case there was probably error.
      []
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


%%@doc
%% Creates a list of diagnostics form the RefactorErl results and a message
%% Message can be the same, as this is called per diagnsotic type.
%% The severity is only warning.


%%==============================================================================
%% Internal Functions
%%==============================================================================

%-spec disable_node(atom()) -> {error, disabled}.
%%@doc
%% Disables the node given in paramter, also notifes the user.
%%disable_node(Node) ->
%%  Msg = "RefactorErl is disconnected!
%%  Reload ELS after you fixed theRefactorErl node!",
%%  notification(Msg, ?MESSAGE_TYPE_ERROR),
%%
%%  Config = els_config:get(refactorerl),
%%  els_config:set(refactorerl, Config#{"node" => {Node, disabled}}),
%%  {error, disabled}.

-spec is_refactorerl(atom()) -> boolean().
is_refactorerl(Node) ->
  case pc:call(Node, referl_els, ping, [], 500) of
    {refactorerl_els, pong} -> true;
    _ -> false
  end.


%%@doc
%% Calls the RefactorErl node, to check if it's alive using ri:ls()
-spec check_node(atom()) -> error
                          | atom().
check_node(Node) ->
  case is_refactorerl(Node) of
    true ->
      Node;
    false ->
      error
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
  Config = els_config:get(refactorerl),
  case {Status, check_node(Node)} of
    {validate, {error, _}} ->
      notification("RefactorErl is not connected!", ?MESSAGE_TYPE_INFO),
      els_config:set(refactorerl, Config#{"node" => {Node, disconnected}}),
      {error, disconnected};
    {retry, {error, _}} ->
      els_config:set(refactorerl, Config#{"node" => {Node, disconnected}}),
      {error, disconnected};
    {_, Node} ->
      notification("RefactorErl is connected!", ?MESSAGE_TYPE_INFO),
      els_config:set(refactorerl, Config#{"node" => {Node, validated}}),
      {ok, Node}
  end.

%%==============================================================================
%% Values
%%==============================================================================

%%@doc
%% Common soruce name for all RefactorErl based backend(s)
-spec source_name() -> binary().
source_name() ->
  <<"RefactorErl">>.