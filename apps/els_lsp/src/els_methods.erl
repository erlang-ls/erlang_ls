-module(els_methods).

-export([dispatch/4]).

-export([
    exit/2,
    initialize/2,
    initialized/2,
    shutdown/2,
    textdocument_completion/2,
    completionitem_resolve/2,
    textdocument_didopen/2,
    textdocument_didchange/2,
    textdocument_didsave/2,
    textdocument_didclose/2,
    textdocument_documentsymbol/2,
    textdocument_hover/2,
    textdocument_definition/2,
    textdocument_implementation/2,
    textdocument_references/2,
    textdocument_documenthighlight/2,
    textdocument_formatting/2,
    textdocument_rangeformatting/2,
    textdocument_ontypeformatting/2,
    textdocument_foldingrange/2,
    workspace_didchangeconfiguration/2,
    textdocument_codeaction/2,
    textdocument_codelens/2,
    textdocument_rename/2,
    textdocument_preparerename/2,
    textdocument_preparecallhierarchy/2,
    textdocument_semantictokens_full/2,
    textdocument_signaturehelp/2,
    callhierarchy_incomingcalls/2,
    callhierarchy_outgoingcalls/2,
    workspace_executecommand/2,
    workspace_didchangewatchedfiles/2,
    workspace_symbol/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-type method_name() :: binary().
-type params() :: map().
-type result() ::
    {response, params() | null, els_server:state()}
    | {error, params(), els_server:state()}
    | {noresponse, els_server:state()}
    | {noresponse, uri(), pid(), els_server:state()}
    | {notification, binary(), params(), els_server:state()}
    | {diagnostics, uri(), [pid()], els_server:state()}
    | {async, uri(), pid(), els_server:state()}.
-type request_type() :: notification | request.

%%==============================================================================
%% @doc Dispatch the handling of the method to els_method
%%==============================================================================
-spec dispatch(method_name(), params(), request_type(), els_server:state()) -> result().
dispatch(<<"$/", Method/binary>>, Params, notification, State) ->
    Msg = "Ignoring $/ notification [method=~p] [params=~p]",
    Fmt = [Method, Params],
    ?LOG_DEBUG(Msg, Fmt),
    {noresponse, State};
dispatch(<<"$/", Method/binary>>, Params, request, State) ->
    Msg = "Ignoring $/ request [method=~p] [params=~p]",
    Fmt = [Method, Params],
    ?LOG_DEBUG(Msg, Fmt),
    Error = #{
        code => ?ERR_METHOD_NOT_FOUND,
        message => <<"Method not found: ", Method/binary>>
    },
    {error, Error, State};
dispatch(Method, Params, MessageType, State) ->
    Function = method_to_function_name(Method),
    ?LOG_DEBUG("Dispatching request [method=~p] [params=~p]", [Method, Params]),
    try
        do_dispatch(Function, Params, State)
    catch
        error:undef ->
            not_implemented_method(Method, State);
        Type:Reason:Stack ->
            ?LOG_ERROR(
                "Internal [type=~p] [error=~p] [stack=~p]",
                [Type, Reason, Stack]
            ),
            case MessageType of
                request ->
                    Error = #{
                        type => Type,
                        reason => Reason,
                        stack => Stack,
                        method => Method,
                        params => Params
                    },
                    ErrorMsg = els_utils:to_binary(lists:flatten(io_lib:format("~p", [Error]))),
                    ErrorResponse = #{
                        code => ?ERR_INTERNAL_ERROR,
                        message => <<"Internal Error: ", ErrorMsg/binary>>
                    },
                    {error, ErrorResponse, State};
                notification ->
                    {noresponse, State}
            end
    end.

-spec do_dispatch(atom(), params(), els_server:state()) -> result().
do_dispatch(exit, Params, State) ->
    els_methods:exit(Params, State);
do_dispatch(_Function, _Params, #{status := shutdown} = State) ->
    Message = <<"Server is shutting down">>,
    Result = #{
        code => ?ERR_INVALID_REQUEST,
        message => Message
    },
    {error, Result, State};
do_dispatch(initialize, Params, State) ->
    els_methods:initialize(Params, State);
do_dispatch(Function, Params, #{status := initialized} = State) ->
    els_methods:Function(Params, State);
do_dispatch(_Function, _Params, State) ->
    Message = <<"The server is not fully initialized yet, please wait.">>,
    Result = #{
        code => ?ERR_SERVER_NOT_INITIALIZED,
        message => Message
    },
    {error, Result, State}.

-spec not_implemented_method(method_name(), els_server:state()) -> result().
not_implemented_method(Method, State) ->
    ?LOG_WARNING("[Method not implemented] [method=~s]", [Method]),
    Message = <<"Method not implemented: ", Method/binary>>,
    Method1 = <<"window/showMessage">>,
    Params = #{
        type => ?MESSAGE_TYPE_INFO,
        message => Message
    },
    {notification, Method1, Params, State}.

-spec method_to_function_name(method_name()) -> atom().
method_to_function_name(<<"$/", Method/binary>>) ->
    method_to_function_name(<<"$_", Method/binary>>);
method_to_function_name(Method) ->
    Replaced = string:replace(Method, <<"/">>, <<"_">>, all),
    Lower = string:lowercase(Replaced),
    Binary = els_utils:to_binary(Lower),
    binary_to_atom(Binary, utf8).

%%==============================================================================
%% Initialize
%%==============================================================================

-spec initialize(params(), els_server:state()) -> result().
initialize(Params, State) ->
    Provider = els_general_provider,
    Request = {initialize, Params},
    {response, Response} = els_provider:handle_request(Provider, Request),
    {response, Response, State#{status => initialized}}.

%%==============================================================================
%% Initialized
%%==============================================================================

-spec initialized(params(), els_server:state()) -> result().
initialized(Params, State) ->
    Provider = els_general_provider,
    Request = {initialized, Params},
    {response, _Response} = els_provider:handle_request(Provider, Request),
    %% Report to the user the server version
    {ok, Version} = application:get_key(?APP, vsn),
    ?LOG_INFO("initialized: [App=~p] [Version=~p]", [?APP, Version]),
    BinVersion = els_utils:to_binary(Version),
    Root = filename:basename(els_uri:path(els_config:get(root_uri))),
    OTPVersion = els_utils:to_binary(erlang:system_info(otp_release)),
    Message =
        <<"Erlang LS (in ", Root/binary, "), version: ", BinVersion/binary, ", OTP version: ",
            OTPVersion/binary>>,
    NMethod = <<"window/showMessage">>,
    NParams = #{
        type => ?MESSAGE_TYPE_INFO,
        message => Message
    },
    {notification, NMethod, NParams, State}.

%%==============================================================================
%% shutdown
%%==============================================================================

-spec shutdown(params(), els_server:state()) -> result().
shutdown(Params, State) ->
    Provider = els_general_provider,
    Request = {shutdown, Params},
    {response, Response} = els_provider:handle_request(Provider, Request),
    {response, Response, State#{status => shutdown}}.

%%==============================================================================
%% exit
%%==============================================================================

-spec exit(params(), els_server:state()) -> no_return().
exit(_Params, State) ->
    Provider = els_general_provider,
    Request = {exit, #{status => maps:get(status, State)}},
    {response, _Response} = els_provider:handle_request(Provider, Request),
    %% Only reached by property-based test (where halt/1 is mocked for
    %% faster iteration
    {noresponse, State#{status => exiting}}.

%%==============================================================================
%% textDocument/didopen
%%==============================================================================

-spec textdocument_didopen(params(), els_server:state()) -> result().
textdocument_didopen(Params, #{open_buffers := OpenBuffers} = State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    Provider = els_text_synchronization_provider,
    Request = {did_open, Params},
    {diagnostics, Uri, Jobs} = els_provider:handle_request(Provider, Request),
    {diagnostics, Uri, Jobs, State#{open_buffers => sets:add_element(Uri, OpenBuffers)}}.

%%==============================================================================
%% textDocument/didchange
%%==============================================================================

-spec textdocument_didchange(params(), els_server:state()) -> result().
textdocument_didchange(Params, State0) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    State = cancel_request_by_uri(Uri, State0),
    Provider = els_text_synchronization_provider,
    Request = {did_change, Params},
    _ = els_provider:handle_request(Provider, Request),
    {noresponse, State}.

%%==============================================================================
%% textDocument/didsave
%%==============================================================================

-spec textdocument_didsave(params(), els_server:state()) -> result().
textdocument_didsave(Params, State) ->
    Provider = els_text_synchronization_provider,
    Request = {did_save, Params},
    {diagnostics, Uri, Jobs} = els_provider:handle_request(Provider, Request),
    {diagnostics, Uri, Jobs, State}.

%%==============================================================================
%% textDocument/didclose
%%==============================================================================

-spec textdocument_didclose(params(), els_server:state()) -> result().
textdocument_didclose(Params, #{open_buffers := OpenBuffers} = State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    Provider = els_text_synchronization_provider,
    Request = {did_close, Params},
    noresponse = els_provider:handle_request(Provider, Request),
    {noresponse, State#{open_buffers => sets:del_element(Uri, OpenBuffers)}}.

%%==============================================================================
%% textdocument/documentSymbol
%%==============================================================================

-spec textdocument_documentsymbol(params(), els_server:state()) -> result().
textdocument_documentsymbol(Params, State) ->
    Provider = els_document_symbol_provider,
    Request = {document_symbol, Params},
    {response, Response} = els_provider:handle_request(Provider, Request),
    {response, Response, State}.

%%==============================================================================
%% textDocument/hover
%%==============================================================================

-spec textdocument_hover(params(), els_server:state()) -> result().
textdocument_hover(Params, State) ->
    Provider = els_hover_provider,
    {async, Uri, Job} = els_provider:handle_request(Provider, {hover, Params}),
    {async, Uri, Job, State}.

%%==============================================================================
%% textDocument/completion
%%==============================================================================

-spec textdocument_completion(params(), els_server:state()) -> result().
textdocument_completion(Params, State) ->
    Provider = els_completion_provider,
    {async, Uri, Job} =
        els_provider:handle_request(Provider, {completion, Params}),
    {async, Uri, Job, State}.

%%==============================================================================
%% completionItem/resolve
%%==============================================================================

-spec completionitem_resolve(params(), els_server:state()) -> result().
completionitem_resolve(Params, State) ->
    Provider = els_completion_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {resolve, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/definition
%%==============================================================================

-spec textdocument_definition(params(), els_server:state()) -> result().
textdocument_definition(Params, State) ->
    Provider = els_definition_provider,
    case els_provider:handle_request(Provider, {definition, Params}) of
        {response, Response} ->
            {response, Response, State};
        {async, Uri, Job} ->
            {async, Uri, Job, State}
    end.

%%==============================================================================
%% textDocument/references
%%==============================================================================

-spec textdocument_references(params(), els_server:state()) -> result().
textdocument_references(Params, State) ->
    Provider = els_references_provider,
    {async, Uri, Job} = els_provider:handle_request(Provider, {references, Params}),
    {async, Uri, Job, State}.

%%==============================================================================
%% textDocument/documentHightlight
%%==============================================================================

-spec textdocument_documenthighlight(params(), els_server:state()) -> result().
textdocument_documenthighlight(Params, State) ->
    Provider = els_document_highlight_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {document_highlight, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/formatting
%%==============================================================================

-spec textdocument_formatting(params(), els_server:state()) -> result().
textdocument_formatting(Params, State) ->
    Provider = els_formatting_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {document_formatting, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/rangeFormatting
%%==============================================================================

-spec textdocument_rangeformatting(params(), els_server:state()) -> result().
textdocument_rangeformatting(Params, State) ->
    Provider = els_formatting_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {document_rangeformatting, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/onTypeFormatting
%%==============================================================================

-spec textdocument_ontypeformatting(params(), els_server:state()) -> result().
textdocument_ontypeformatting(Params, State) ->
    Provider = els_formatting_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {document_ontypeformatting, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/foldingRange
%%==============================================================================

-spec textdocument_foldingrange(params(), els_server:state()) -> result().
textdocument_foldingrange(Params, State) ->
    Provider = els_folding_range_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {document_foldingrange, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/implementation
%%==============================================================================

-spec textdocument_implementation(params(), els_server:state()) -> result().
textdocument_implementation(Params, State) ->
    Provider = els_implementation_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {implementation, Params}),
    {response, Response, State}.

%%==============================================================================
%% workspace/didChangeConfiguration
%%==============================================================================

-spec workspace_didchangeconfiguration(params(), els_server:state()) -> result().
workspace_didchangeconfiguration(_Params, State) ->
    %% Some clients send this notification on startup, even though we
    %% have no server-side config.  So swallow it without complaining.
    {noresponse, State}.

%%==============================================================================
%% textDocument/codeAction
%%==============================================================================

-spec textdocument_codeaction(params(), els_server:state()) -> result().
textdocument_codeaction(Params, State) ->
    Provider = els_code_action_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {document_codeaction, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/codeLens
%%==============================================================================

-spec textdocument_codelens(params(), els_server:state()) -> result().
textdocument_codelens(Params, State) ->
    Provider = els_code_lens_provider,
    {async, Uri, Job} =
        els_provider:handle_request(Provider, {document_codelens, Params}),
    {async, Uri, Job, State}.

%%==============================================================================
%% textDocument/rename
%%==============================================================================

-spec textdocument_rename(params(), els_server:state()) -> result().
textdocument_rename(Params, State) ->
    Provider = els_rename_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {rename, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/prepareRename
%%=============================================================================
-spec textdocument_preparerename(params(), els_server:state()) -> result().
textdocument_preparerename(Params, State) ->
    Provider = els_prepare_rename_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {prepare_rename, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/semanticTokens/full
%%==============================================================================

-spec textdocument_semantictokens_full(params(), els_server:state()) -> result().
textdocument_semantictokens_full(Params, State) ->
    Provider = els_semantic_token_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {semantic_tokens, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/preparePreparecallhierarchy
%%==============================================================================

-spec textdocument_preparecallhierarchy(params(), els_server:state()) -> result().
textdocument_preparecallhierarchy(Params, State) ->
    Provider = els_call_hierarchy_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {prepare, Params}),
    {response, Response, State}.

%%==============================================================================
%% textDocument/signatureHelp
%%==============================================================================

-spec textdocument_signaturehelp(params(), els_server:state()) -> result().
textdocument_signaturehelp(Params, State) ->
    Provider = els_signature_help_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {signature_help, Params}),
    {response, Response, State}.

%%==============================================================================
%% callHierarchy/incomingCalls
%%==============================================================================

-spec callhierarchy_incomingcalls(params(), els_server:state()) -> result().
callhierarchy_incomingcalls(Params, State) ->
    Provider = els_call_hierarchy_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {incoming_calls, Params}),
    {response, Response, State}.

%%==============================================================================
%% callHierarchy/outgoingCalls
%%==============================================================================

-spec callhierarchy_outgoingcalls(params(), els_server:state()) -> result().
callhierarchy_outgoingcalls(Params, State) ->
    Provider = els_call_hierarchy_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {outgoing_calls, Params}),
    {response, Response, State}.

%%==============================================================================
%% workspace/executeCommand
%%==============================================================================

-spec workspace_executecommand(params(), els_server:state()) -> result().
workspace_executecommand(Params, State) ->
    Provider = els_execute_command_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {workspace_executecommand, Params}),
    {response, Response, State}.

%%==============================================================================
%% workspace/didChangeWatchedFiles
%%==============================================================================

-spec workspace_didchangewatchedfiles(map(), els_server:state()) -> result().
workspace_didchangewatchedfiles(Params0, State) ->
    #{open_buffers := OpenBuffers} = State,
    #{<<"changes">> := Changes0} = Params0,
    Changes = [
        C
     || #{<<"uri">> := Uri} = C <- Changes0,
        not sets:is_element(Uri, OpenBuffers)
    ],
    Params = Params0#{<<"changes">> => Changes},
    Provider = els_text_synchronization_provider,
    Request = {did_change_watched_files, Params},
    noresponse = els_provider:handle_request(Provider, Request),
    {noresponse, State}.

%%==============================================================================
%% workspace/symbol
%%==============================================================================

-spec workspace_symbol(map(), els_server:state()) -> result().
workspace_symbol(Params, State) ->
    Provider = els_workspace_symbol_provider,
    {response, Response} =
        els_provider:handle_request(Provider, {symbol, Params}),
    {response, Response, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec cancel_request_by_uri(uri(), els_server:state()) -> els_server:state().
cancel_request_by_uri(Uri, State) ->
    #{in_progress := InProgress0} = State,
    Fun = fun({U, Job}) ->
        case U =:= Uri of
            true ->
                els_background_job:stop(Job),
                false;
            false ->
                true
        end
    end,
    InProgress = lists:filtermap(Fun, InProgress0),
    ?LOG_DEBUG("Cancelling requests by Uri [uri=~p]", [Uri]),
    State#{in_progress => InProgress}.
