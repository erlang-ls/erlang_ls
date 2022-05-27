%%==============================================================================
%% Erlang LS & Refactor Erl communication
%%==============================================================================
-module(els_refactorerl_utils).

%%==============================================================================
%% API
%%==============================================================================
-export([
    referl_node/0,
    notification/1,
    notification/2,
    run_diagnostics/2,
    source_name/0,
    add/1
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
-spec referl_node() ->
    {ok, atom()}
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
            {error, disabled};
        _ ->
            {error, other}
    end.

%%@doc
%% Adds a module to the RefactorErl node. Using the UI router
%% Returns 'ok' if successfull
%% Returns 'error' if it fails
-spec add(uri()) -> error | ok.
add(Uri) ->
    case els_refactorerl_utils:referl_node() of
        {ok, Node} ->
            Path = [binary_to_list(els_uri:path(Uri))],
            %% returns error | ok
            rpc:call(Node, referl_els, add, [Path]);
        _ ->
            error
    end.

%%@doc
%% Runs list of diagnostic aliases on refactorerl
-spec run_diagnostics(list(), atom()) -> list().
run_diagnostics(DiagnosticAliases, Module) ->
    case els_refactorerl_utils:referl_node() of
        {ok, Node} ->
            %% returns error | ok
            rpc:call(Node, referl_els, run_diagnostics, [DiagnosticAliases, Module]);
        % In this case there was probably error.
        _ ->
            []
    end.

%%@doc
%% Util for popping up notifications
-spec notification(string(), number()) -> atom().
notification(Msg, Severity) ->
    Param = #{
        type => Severity,
        message => list_to_binary(Msg)
    },
    els_server:send_notification(<<"window/showMessage">>, Param).

-spec notification(string()) -> atom().
notification(Msg) ->
    notification(Msg, ?MESSAGE_TYPE_INFO).

%%==============================================================================
%% Internal Functions
%%==============================================================================

%%@doc
%% Checks if the given node is running RefactorErl with ELS interface
-spec is_refactorerl(atom()) -> boolean().
is_refactorerl(Node) ->
    case rpc:call(Node, referl_els, ping, [], 500) of
        {refactorerl_els, pong} -> true;
        _ -> false
    end.

%%@doc
%% Tries to connect to a node.
%% When it status is validate, the node hasn't been checked yet,
%% so it will reports the success, and failure as well,
%%
%% when retry, it won't report.
-spec connect_node({validate | retry, atom()}) ->
    {error, disconnected}
    | atom().
connect_node({Status, Node}) ->
    Config = els_config:get(refactorerl),
    case {Status, is_refactorerl(Node)} of
        {validate, false} ->
            els_config:set(refactorerl, Config#{"node" => {Node, disconnected}}),
            {error, disconnected};
        {retry, false} ->
            els_config:set(refactorerl, Config#{"node" => {Node, disconnected}}),
            {error, disconnected};
        {_, true} ->
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
