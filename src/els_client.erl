%%==============================================================================
%% A client for the Erlang Language Server
%%==============================================================================
-module(els_client).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================
%% API
-export([ completion/5
        , definition/3
        , did_open/4
        , did_save/1
        , did_close/1
        , document_symbol/1
        , exit/0
        , hover/3
        , initialize/2
        , references/3
        , document_highlight/3
        , shutdown/0
        , start_link/2
        , stop/0
        , workspace_symbol/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(JSON_OPTS, [return_maps, {labels, atom}]).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { transport :: tcp | stdio
               , connection
               , request_id = 1
               , pending    = []
               , buffer   = <<>>
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()        :: #state{}.
-type hostname()     :: tuple().
-type port_no()      :: pos_integer().
-type init_options() :: [].

%%==============================================================================
%% API
%%==============================================================================
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
  gen_server:call( ?SERVER
                 , {completion, Opts}
                 ).

-spec definition(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
definition(Uri, Line, Char) ->
  gen_server:call(?SERVER, {definition, {Uri, Line, Char}}).

-spec hover(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
hover(Uri, Line, Char) ->
  gen_server:call(?SERVER, {hover, {Uri, Line, Char}}).

-spec references(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
references(Uri, Line, Char) ->
  gen_server:call(?SERVER, {references, {Uri, Line, Char}}).

-spec document_highlight(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
document_highlight(Uri, Line, Char) ->
  gen_server:call(?SERVER, {document_highlight, {Uri, Line, Char}}).

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

-spec initialize(uri(), init_options()) -> map().
initialize(RootUri, InitOptions) ->
  gen_server:call(?SERVER, {initialize, {RootUri, InitOptions}}).

-spec shutdown() -> map().
shutdown() ->
  gen_server:call(?SERVER, {shutdown}).

-spec exit() -> ok.
exit() ->
  gen_server:call(?SERVER, {exit}).

-spec start_link(tcp | stdio, any()) -> {ok, pid()}.
start_link(tcp, {Host, Port}) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {tcp, Host, Port}, []);
start_link(stdio, IoDevice) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {stdio, IoDevice}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec workspace_symbol(string()) ->
  ok.
workspace_symbol(Query) ->
  gen_server:call(?SERVER, {workspace_symbol, {Query}}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({tcp, hostname(), port_no()}) -> {ok, state()}.
init({tcp, Host, Port}) ->
  Opts         = [binary, {active, once}, {packet, 0}],
  {ok, Socket} = gen_tcp:connect(Host, Port, Opts),
  {ok, #state{transport = tcp, connection = Socket}};
init({stdio, IoDevice}) ->
  Self = self(),
  proc_lib:spawn_link(els_stdio, loop, [[], IoDevice, Self, ?JSON_OPTS]),
  {ok, #state{transport = stdio, connection = IoDevice}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({Action, Opts}, _From, State) when Action =:= did_save
                                        orelse Action =:= did_close
                                        orelse Action =:= did_open ->
  Method = method_lookup(Action),
  Params = notification_params(Opts),
  Content = els_protocol:notification(Method, Params),
  send(Content, State),
  {reply, ok, State};
handle_call({exit}, _From, State) ->
  RequestId = State#state.request_id,
  Method = <<"exit">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {reply, ok, State};
handle_call({shutdown}, From, State) ->
  RequestId = State#state.request_id,
  Method = <<"shutdown">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call(Input = {Action, _}, From, State) ->
  #state{request_id = RequestId} = State,
  Method = method_lookup(Action),
  Params = request_params(Input),
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast( {process_requests, Responses}
           , #state{pending = Pending, transport = stdio} = State
           ) ->
  Pending1 = handle_responses(Responses, Pending),
  {noreply, State#state{pending = Pending1}};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({tcp, _Socket, Packet}, #state{ buffer    = Buffer
                                          , pending   = Pending
                                          , transport = tcp
                                          } = State) ->
  inet:setopts(State#state.connection, [{active, once}]),
  {Responses, NewBuffer} = process_packet(Buffer, Packet),
  Pending1 = handle_responses(Responses, Pending),
  {noreply, State#state{ buffer = NewBuffer, pending = Pending1 }};
handle_info({tcp_closed, _Socket}, State) ->
  lager:debug("[CLIENT] TCP closed", []),
  {noreply, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
  lager:debug("[CLIENT] TCP error [reason=~p]", [Reason]),
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate( _Reason
         , #state{transport = tcp, connection = Socket} = _State
         ) ->
  ok = gen_tcp:close(Socket);
terminate( _Reason
         , #state{transport = stdio, connection = IoDevice} = _State
         ) ->
  true = erlang:exit(IoDevice, normal),
  ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec process_packet(binary(), binary()) -> {[map()], binary()}.
process_packet(Buffer, Packet) ->
  lager:debug("[SERVER] TCP Packet [buffer=~p] [packet=~p] ", [ Buffer
                                                              , Packet]),
  Data = <<Buffer/binary, Packet/binary>>,
  els_jsonrpc:split(Data, ?JSON_OPTS).

-spec handle_responses([map()], [any()]) -> [any()].
handle_responses([], Pending) ->
  Pending;
handle_responses([Response|Responses], Pending) ->
  case maps:find(id, Response) of
    {ok, RequestId} ->
      lager:debug("[CLIENT] Handling Response [response=~p]", [Response]),
      case lists:keyfind(RequestId, 1, Pending) of
        {RequestId, From} ->
          gen_server:reply(From, Response),
          handle_responses( Responses
                          , lists:keydelete(RequestId, 1, Pending));
        false ->
          handle_responses(Responses, Pending)
      end;
    error ->
      lager:debug( "[CLIENT] Handling Notification [notification=~p]"
                 , [Response]),
      handle_responses(Responses, Pending)
  end.

-spec send(iolist(), state()) -> any().
send(Content, #state{transport = tcp, connection = Socket}) ->
  gen_tcp:send(Socket, Content);
send(Content, #state{transport = stdio, connection = IoDevice}) ->
  io:format(IoDevice, iolist_to_binary(Content), []).

-spec method_lookup(atom()) -> binary().
method_lookup(completion)         -> <<"textDocument/completion">>;
method_lookup(definition)         -> <<"textDocument/definition">>;
method_lookup(document_symbol)    -> <<"textDocument/documentSymbol">>;
method_lookup(references)         -> <<"textDocument/references">>;
method_lookup(document_highlight) -> <<"textDocument/documentHighlight">>;
method_lookup(did_open)           -> <<"textDocument/didOpen">>;
method_lookup(did_save)           -> <<"textDocument/didSave">>;
method_lookup(did_close)          -> <<"textDocument/didClose">>;
method_lookup(hover)              -> <<"textDocument/hover">>;
method_lookup(workspace_symbol)   -> <<"workspace/symbol">>;
method_lookup(initialize)         -> <<"initialize">>.

-spec request_params(tuple()) -> any().
request_params({document_symbol, {Uri}}) ->
  TextDocument = #{ uri => Uri },
  #{ textDocument => TextDocument };
request_params({workspace_symbol, {Query}}) ->
  #{ query => Query };
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
  #{textDocument => TextDocument}.
