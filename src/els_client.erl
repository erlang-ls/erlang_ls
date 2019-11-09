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
  gen_server:call( ?SERVER
                 , {completion, Uri, Line, Char, TriggerKind, TriggerCharacter}
                 ).

-spec definition(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
definition(Uri, Line, Char) ->
  gen_server:call(?SERVER, {definition, Uri, Line, Char}).

-spec hover(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
hover(Uri, Line, Char) ->
  gen_server:call(?SERVER, {hover, Uri, Line, Char}).

-spec references(uri(), non_neg_integer(), non_neg_integer()) ->
  ok.
references(Uri, Line, Char) ->
  gen_server:call(?SERVER, {references, Uri, Line, Char}).

-spec did_open(uri(), binary(), number(), binary()) -> ok.
did_open(Uri, LanguageId, Version, Text) ->
  gen_server:call(?SERVER, {did_open, Uri, LanguageId, Version, Text}).

-spec did_save(uri()) -> ok.
did_save(Uri) ->
  gen_server:call(?SERVER, {did_save, Uri}).

-spec did_close(uri()) -> ok.
did_close(Uri) ->
  gen_server:call(?SERVER, {did_close, Uri}).

-spec document_symbol(uri()) ->
  ok.
document_symbol(Uri) ->
  gen_server:call(?SERVER, {document_symbol, Uri}).

-spec initialize(uri(), init_options()) -> map().
initialize(RootUri, InitOptions) ->
  gen_server:call(?SERVER, {initialize, RootUri, InitOptions}).

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
  gen_server:call(?SERVER, {workspace_symbol, Query}).

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
handle_call( {completion, Uri, Line, Char, TriggerKind, TriggerCharacter}
           , From
           , State) ->
  #state{request_id = RequestId} = State,
  Method = <<"textDocument/completion">>,
  Params = #{ position     => #{ line      => Line - 1
                               , character => Char - 1
                               }
            , textDocument => #{ uri  => Uri }
            , context      => #{ triggerKind      => TriggerKind
                               , triggerCharacter => TriggerCharacter
                               }
            },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({definition, Uri, Line, Char}, From, State) ->
  #state{request_id = RequestId} = State,
  Method = <<"textDocument/definition">>,
  TextDocument = #{ uri  => Uri },
  Position = #{ line      => Line - 1
              , character => Char - 1
              },
  Params = #{ position     => Position
            , textDocument => TextDocument
            },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({document_symbol, Uri}, From, State) ->
  RequestId = State#state.request_id,
  Method = <<"textDocument/documentSymbol">>,
  TextDocument = #{ uri  => Uri },
  Params = #{ textDocument => TextDocument },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({references, Uri, Line, Char}, From, State) ->
  RequestId = State#state.request_id,
  Method = <<"textDocument/references">>,
  Params = #{ position     => #{ line      => Line - 1
                               , character => Char - 1
                               }
            , textDocument => #{ uri  => Uri }
            },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({did_open, Uri, LanguageId, Version, Text}, _From, State) ->
  Method = <<"textDocument/didOpen">>,
  TextDocument = #{ uri        => Uri
                  , languageId => LanguageId
                  , version    => Version
                  , text       => Text
                  },
  Params = #{textDocument => TextDocument},
  Content = els_protocol:notification(Method, Params),
  send(Content, State),
  {reply, ok, State};
handle_call({did_save, Uri}, _From, State) ->
  Method = <<"textDocument/didSave">>,
  TextDocument = #{ uri => Uri },
  Params = #{textDocument => TextDocument},
  Content = els_protocol:notification(Method, Params),
  send(Content, State),
  {reply, ok, State};
handle_call({did_close, Uri}, _From, State) ->
  Method = <<"textDocument/didClose">>,
  TextDocument = #{ uri => Uri },
  Params = #{textDocument => TextDocument},
  Content = els_protocol:notification(Method, Params),
  send(Content, State),
  {reply, ok, State};
handle_call({hover, Uri, Line, Char}, From, State) ->
  RequestId = State#state.request_id,
  Method = <<"textDocument/hover">>,
  TextDocument = #{ uri  => Uri },
  Position = #{ line      => Line - 1
              , character => Char - 1
              },
  Params = #{ position     => Position
            , textDocument => TextDocument
            },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({initialize, RootUri, InitOptions}, From, State) ->
  RequestId = State#state.request_id,
  Method  = <<"initialize">>,
  Params  = #{ <<"rootUri">> => RootUri
             , <<"initializationOptions">> => InitOptions
             , <<"capabilities">> =>
                 #{ <<"textDocument">> =>
                      #{ <<"completion">> =>
                           #{ <<"contextSupport">> => 'true' }
                       }
                  , <<"hover">> =>
                      #{ <<"contentFormat">> => [ ?MARKDOWN
                                                , ?PLAINTEXT
                                                ]
                       }
                  }
             },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({workspace_symbol, Query}, From, State) ->
  RequestId = State#state.request_id,
  Method = <<"workspace/symbol">>,
  Params = #{ query => Query },
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({shutdown}, From, State) ->
  RequestId = State#state.request_id,
  Method = <<"shutdown">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({exit}, _From, State) ->
  RequestId = State#state.request_id,
  Method = <<"exit">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(Content, State),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast( {messages, Responses}
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
