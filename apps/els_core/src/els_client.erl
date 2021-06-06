%%==============================================================================
%% A client for the Erlang Language Server
%%==============================================================================
-module(els_client).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).
%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , code_change/3
        ]).

%%==============================================================================
%% Exports
%%==============================================================================
%% API
-export([ '$_cancelrequest'/0
        , '$_cancelrequest'/1
        , '$_settracenotification'/0
        , '$_unexpectedrequest'/0
        , completion/5
        , completionitem_resolve/1
        , definition/3
        , did_open/4
        , did_save/1
        , did_close/1
        , document_symbol/1
        , exit/0
        , hover/3
        , implementation/3
        , initialize/1
        , initialize/2
        , initialized/0
        , references/3
        , document_highlight/3
        , document_codeaction/3
        , document_codelens/1
        , document_formatting/3
        , document_rangeformatting/3
        , document_ontypeformatting/4
        , document_rename/4
        , folding_range/1
        , shutdown/0
        , start_link/1
        , stop/0
        , workspace_symbol/1
        , workspace_executecommand/2

        , get_notifications/0
        ]).

-export([ handle_responses/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("els_core/include/els_core.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, infinity).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { io_device        :: atom() | pid()
               , request_id       = 1 :: request_id()
               , pending          = []
               , notifications    = []
               , requests         = []
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()        :: #state{}.
-type init_options() :: #{}.
-type request_id()   :: pos_integer().

%%==============================================================================
%% API
%%==============================================================================
-spec '$_cancelrequest'() -> ok.
'$_cancelrequest'() ->
  gen_server:call(?SERVER, {'$_cancelrequest'}).

-spec '$_cancelrequest'(request_id()) -> ok.
'$_cancelrequest'(Id) ->
  gen_server:call(?SERVER, {'$_cancelrequest', Id}).

-spec '$_settracenotification'() -> ok.
'$_settracenotification'() ->
  gen_server:call(?SERVER, {'$_settracenotification'}).

-spec '$_unexpectedrequest'() -> ok.
'$_unexpectedrequest'() ->
  gen_server:call(?SERVER, {'$_unexpectedrequest'}).

%% TODO: More accurate and consistent parameters list
-spec completion( uri()
                , non_neg_integer()
                , non_neg_integer()
                , integer()
                , binary()
                ) ->
  ok.
completion(Uri, Line, Char, TriggerKind, TriggerCharacter) ->
  Opts = {Uri, Line, Char, TriggerKind, TriggerCharacter},
  gen_server:call(?SERVER, {completion, Opts}).

-spec completionitem_resolve(completion_item()) -> ok.
completionitem_resolve(CompletionItem) ->
  gen_server:call(?SERVER, {completionitem_resolve, CompletionItem}).

-spec definition(uri(), non_neg_integer(), non_neg_integer()) -> ok.
definition(Uri, Line, Char) ->
  gen_server:call(?SERVER, {definition, {Uri, Line, Char}}).

-spec hover(uri(), non_neg_integer(), non_neg_integer()) -> ok.
hover(Uri, Line, Char) ->
  gen_server:call(?SERVER, {hover, {Uri, Line, Char}}).

-spec implementation(uri(), non_neg_integer(), non_neg_integer()) -> ok.
implementation(Uri, Line, Char) ->
  gen_server:call(?SERVER, {implementation, {Uri, Line, Char}}).

-spec references(uri(), non_neg_integer(), non_neg_integer()) -> ok.
references(Uri, Line, Char) ->
  gen_server:call(?SERVER, {references, {Uri, Line, Char}}).

-spec document_highlight(uri(), non_neg_integer(), non_neg_integer()) -> ok.
document_highlight(Uri, Line, Char) ->
  gen_server:call(?SERVER, {document_highlight, {Uri, Line, Char}}).

-spec document_codeaction(uri(), range(), [els_diagnostics:diagnostic()]) -> ok.
document_codeaction(Uri, Range, Diagnostics) ->
  gen_server:call(?SERVER, {document_codeaction, {Uri, Range, Diagnostics}}).

-spec document_codelens(uri()) -> ok.
document_codelens(Uri) ->
  gen_server:call(?SERVER, {document_codelens, {Uri}}).

-spec document_formatting(uri(), non_neg_integer(), boolean()) ->
  ok.
document_formatting(Uri, TabSize, InsertSpaces) ->
  gen_server:call(?SERVER, {document_formatting, {Uri, TabSize, InsertSpaces}}).

-spec document_rangeformatting(uri(), range(), formatting_options()) ->
  ok.
document_rangeformatting(Uri, Range, FormattingOptions) ->
  gen_server:call(?SERVER, {document_rangeformatting,
                            {Uri, Range, FormattingOptions}}).

-spec document_ontypeformatting(uri(), position(), string()
                               , formatting_options()) ->
  ok.
document_ontypeformatting(Uri, Position, Char, FormattingOptions) ->
  gen_server:call(?SERVER, {document_ontypeformatting,
                            {Uri, Position, Char, FormattingOptions}}).

-spec document_rename(uri(), non_neg_integer(), non_neg_integer(), binary()) ->
        ok.
document_rename(Uri, Line, Character, NewName) ->
  gen_server:call(?SERVER, {rename, {Uri, Line, Character, NewName}}).

-spec did_open(uri(), binary(), number(), binary()) -> ok.
did_open(Uri, LanguageId, Version, Text) ->
  gen_server:call(?SERVER, {did_open, {Uri, LanguageId, Version, Text}}).

-spec did_save(uri()) -> ok.
did_save(Uri) ->
  gen_server:call(?SERVER, {did_save, {Uri}}).

-spec did_close(uri()) -> ok.
did_close(Uri) ->
  gen_server:call(?SERVER, {did_close, {Uri}}).

-spec document_symbol(uri()) ->
  ok.
document_symbol(Uri) ->
  gen_server:call(?SERVER, {document_symbol, {Uri}}).

-spec folding_range(uri()) -> ok.
folding_range(Uri) ->
  gen_server:call(?SERVER, {folding_range, {Uri}}).

-spec initialize(uri()) -> map().
initialize(RootUri) ->
  initialize(RootUri, #{}).

-spec initialize(uri(), init_options()) -> map().
initialize(RootUri, InitOptions) ->
  gen_server:call(?SERVER, {initialize, {RootUri, InitOptions}}, ?TIMEOUT).

-spec initialized() -> map().
initialized() ->
  gen_server:call(?SERVER, {initialized, {}}).

-spec shutdown() -> map().
shutdown() ->
  gen_server:call(?SERVER, {shutdown}).

-spec exit() -> ok.
exit() ->
  gen_server:call(?SERVER, {exit}).

-spec start_link(any()) -> {ok, pid()}.
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec workspace_symbol(string()) ->
  ok.
workspace_symbol(Query) ->
  gen_server:call(?SERVER, {workspace_symbol, {Query}}).

-spec workspace_executecommand(string(), [map()]) ->
  ok.
workspace_executecommand(Command, Args) ->
  gen_server:call(?SERVER, {workspace_executecommand, {Command, Args}}).

-spec get_notifications() -> [any()].
get_notifications() ->
  gen_server:call(?SERVER, {get_notifications}).

-spec handle_responses([map()]) -> ok.
handle_responses(Responses) ->
  gen_server:cast(?SERVER, {handle_responses, Responses}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init(any()) -> {ok, state()}.
init(#{io_device := IoDevice}) ->
  Args = [ []
         , IoDevice
         , fun handle_responses/1
         , els_jsonrpc:default_opts()
         ],
  _Pid = proc_lib:spawn_link(els_stdio, loop, Args),
  State = #state{io_device = IoDevice},
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({Action, Opts}, _From, State) when Action =:= did_save
                                               orelse Action =:= did_close
                                               orelse Action =:= did_open
                                               orelse Action =:= initialized ->
  #state{io_device = IoDevice} = State,
  Method = method_lookup(Action),
  Params = notification_params(Opts),
  Content = els_protocol:notification(Method, Params),
  send(IoDevice, Content),
  {reply, ok, State};
handle_call({exit}, _From, State) ->
  #state{io_device = IoDevice} = State,
  RequestId = State#state.request_id,
  Method = <<"exit">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(IoDevice, Content),
  {reply, ok, State};
handle_call({shutdown}, From, State) ->
  #state{io_device = IoDevice} = State,
  RequestId = State#state.request_id,
  Method = <<"shutdown">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(IoDevice, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({'$_cancelrequest'}, _From, State) ->
  #state{request_id = Id} = State,
  do_cancel_request(Id - 1, State),
  {reply, ok, State};
handle_call({'$_cancelrequest', Id}, _From, State) ->
  do_cancel_request(Id, State),
  {reply, ok, State};
handle_call({'$_settracenotification'}, _From, State) ->
  #state{io_device = IoDevice} = State,
  Method = <<"$/setTraceNotification">>,
  Params = #{value => <<"verbose">>},
  Content = els_protocol:notification(Method, Params),
  send(IoDevice, Content),
  {reply, ok, State};
handle_call({'$_unexpectedrequest'}, From, State) ->
  #state{io_device = IoDevice} = State,
  RequestId = State#state.request_id,
  Method = <<"$/unexpectedRequest">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(IoDevice, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({get_notifications}, _From, State) ->
  #state{notifications = Notifications} = State,
  {reply, Notifications, State#state { notifications = []}};
handle_call(Input = {Action, _}, From, State) ->
  #state{ io_device = IoDevice
        , request_id = RequestId
        } = State,
  Method = method_lookup(Action),
  Params = request_params(Input),
  Content = els_protocol:request(RequestId, Method, Params),
  send(IoDevice, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({handle_responses, Responses}, State) ->
  #state{ pending = Pending0
        , notifications = Notifications0
        , requests = Requests0
        } = State,
  {Pending, Notifications, Requests}
    = do_handle_messages(Responses, Pending0, Notifications0, Requests0),
  {noreply, State#state{ pending = Pending
                       , notifications = Notifications
                       , requests = Requests
                       }};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc handle messages received from the transport layer
-spec do_handle_messages([map()], [any()], [any()], [any()]) ->
        {[any()], [any()], [any()]}.
do_handle_messages([], Pending, Notifications, Requests) ->
  {Pending, Notifications, Requests};
do_handle_messages([Message|Messages], Pending, Notifications, Requests) ->
  case is_response(Message) of
    true ->
      RequestId = maps:get(id, Message),
      ?LOG_DEBUG("[CLIENT] Handling Response [response=~p]", [Message]),
      case lists:keyfind(RequestId, 1, Pending) of
        {RequestId, From} ->
          gen_server:reply(From, Message),
          do_handle_messages( Messages
                            , lists:keydelete(RequestId, 1, Pending)
                            , Notifications
                            , Requests
                            );
        false ->
          do_handle_messages(Messages, Pending, Notifications, Requests)
      end;
    false ->
      case is_notification(Message) of
        true ->
          ?LOG_DEBUG( "[CLIENT] Discarding Notification [message=~p]"
                    , [Message]),
          do_handle_messages( Messages
                            , Pending
                            , [Message|Notifications]
                            , Requests);
        false ->
          ?LOG_DEBUG( "[CLIENT] Discarding Server Request [message=~p]"
                    , [Message]),
          do_handle_messages( Messages
                            , Pending
                            , Notifications
                            , [Message|Requests])
      end
  end.

-spec method_lookup(atom()) -> binary().
method_lookup(completion)               -> <<"textDocument/completion">>;
method_lookup(completionitem_resolve)   -> <<"completionItem/resolve">>;
method_lookup(definition)               -> <<"textDocument/definition">>;
method_lookup(document_symbol)          -> <<"textDocument/documentSymbol">>;
method_lookup(references)               -> <<"textDocument/references">>;
method_lookup(document_highlight)       -> <<"textDocument/documentHighlight">>;
method_lookup(document_codeaction)      -> <<"textDocument/codeAction">>;
method_lookup(document_codelens)        -> <<"textDocument/codeLens">>;
method_lookup(document_formatting)      -> <<"textDocument/formatting">>;
method_lookup(document_rangeformatting) -> <<"textDocument/rangeFormatting">>;
method_lookup(document_ontypeormatting) -> <<"textDocument/onTypeFormatting">>;
method_lookup(rename)                   -> <<"textDocument/rename">>;
method_lookup(did_open)                 -> <<"textDocument/didOpen">>;
method_lookup(did_save)                 -> <<"textDocument/didSave">>;
method_lookup(did_close)                -> <<"textDocument/didClose">>;
method_lookup(hover)                    -> <<"textDocument/hover">>;
method_lookup(implementation)           -> <<"textDocument/implementation">>;
method_lookup(workspace_symbol)         -> <<"workspace/symbol">>;
method_lookup(workspace_executecommand) -> <<"workspace/executeCommand">>;
method_lookup(folding_range)            -> <<"textDocument/foldingRange">>;
method_lookup(initialize)               -> <<"initialize">>;
method_lookup(initialized)              -> <<"initialized">>.

-spec request_params(tuple()) -> any().
request_params({document_symbol, {Uri}}) ->
  TextDocument = #{ uri => Uri },
  #{ textDocument => TextDocument };
request_params({workspace_symbol, {Query}}) ->
  #{ query => Query };
request_params({workspace_executecommand, {Command, Args}}) ->
  #{ command   => Command
   , arguments => Args };
request_params({ completion
               , {Uri, Line, Char, TriggerKind, TriggerCharacter}}) ->
  #{ textDocument => #{ uri => Uri }
   , position     => #{ line      => Line - 1
                      , character => Char - 1
                      }
   , context => #{ triggerKind      => TriggerKind
                 , triggerCharacter => TriggerCharacter
                 }
   };
request_params({completionitem_resolve, CompletionItem}) ->
  CompletionItem;
request_params({initialize, {RootUri, InitOptions}}) ->
  ContentFormat = [ ?MARKDOWN , ?PLAINTEXT ],
  TextDocument = #{ <<"completion">> =>
                    #{ <<"contextSupport">> => 'true' }
                  , <<"hover">> =>
                    #{ <<"contentFormat">> => ContentFormat }
                  },
  #{ <<"rootUri">> => RootUri
   , <<"initializationOptions">> => InitOptions
   , <<"capabilities">> => #{ <<"textDocument">> => TextDocument }
   };
request_params({ document_codeaction, {Uri, Range, Diagnostics}}) ->
  #{ textDocument => #{ uri => Uri }
   , range        => Range
   , context      => #{ diagnostics => Diagnostics }
   };
request_params({ document_codelens, {Uri}}) ->
  #{ textDocument => #{ uri => Uri }};
request_params({ document_formatting
               , {Uri, TabSize, InsertSpaces}}) ->
  #{ textDocument => #{ uri => Uri }
   , options      => #{ tabSize      => TabSize
                      , insertSpaces => InsertSpaces
                      }
   };
request_params({rename, {Uri, Line, Character, NewName}}) ->
  #{ textDocument => #{ uri => Uri }
   , position     => #{ line      => Line
                      , character => Character
                      }
   , newName      => NewName
   };
request_params({folding_range, {Uri}}) ->
  TextDocument = #{ uri => Uri },
  #{ textDocument => TextDocument };
request_params({_Action, {Uri, Line, Char}}) ->
  #{ textDocument => #{ uri => Uri }
   , position     => #{ line      => Line - 1
                      , character => Char - 1
                      }
   }.

-spec notification_params(tuple()) -> map().
notification_params({Uri}) ->
  TextDocument = #{ uri => Uri },
  #{textDocument => TextDocument};
notification_params({Uri, LanguageId, Version, Text}) ->
  TextDocument = #{ uri        => Uri
                  , languageId => LanguageId
                  , version    => Version
                  , text       => Text
                  },
  #{textDocument => TextDocument};
notification_params({}) ->
  #{}.

-spec is_notification(map()) -> boolean().
is_notification(#{id := _Id}) ->
  false;
is_notification(_) ->
  true.

-spec is_response(map()) -> boolean().
is_response(#{method := _Method}) ->
  false;
is_response(_) ->
  true.

-spec do_cancel_request(request_id(), state()) -> ok.
do_cancel_request(Id, State) ->
  #state{io_device = IoDevice} = State,
  Method = <<"$/cancelRequest">>,
  Params = #{id => Id},
  Content = els_protocol:notification(Method, Params),
  send(IoDevice, Content).

-spec send(atom() | pid(), binary()) -> ok.
send(IoDevice, Payload) ->
  els_stdio:send(IoDevice, Payload).
