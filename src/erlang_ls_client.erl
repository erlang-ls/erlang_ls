%%==============================================================================
%% A client for the Erlang Language Server
%%==============================================================================
-module(erlang_ls_client).

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
        , initialize/2
        , references/3
        , start_link/2
        , stop/0
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

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { socket
               , request_id = 1
               , pending    = []
               , document   = <<>>
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

-spec initialize(uri(), init_options()) -> ok.
initialize(RootUri, InitOptions) ->
  gen_server:call(?SERVER, {initialize, RootUri, InitOptions}).

-spec start_link(hostname(), port_no()) -> {ok, pid()}.
start_link(Host, Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Host, Port}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({hostname(), port_no()}) -> {ok, state()}.
init({Host, Port}) ->
  Opts         = [binary, {active, once}, {packet, 0}],
  {ok, Socket} = gen_tcp:connect(Host, Port, Opts),
  {ok, #state{socket = Socket}}.

%% TODO: Refactor request function
-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({completion, Uri, Line, Char, TriggerKind, TriggerCharacter}, From, State) ->
  #state{ request_id = RequestId
        , socket     = Socket
        } = State,
  Method = <<"textDocument/completion">>,
  Params = #{ position     => #{ line      => Line - 1
                               , character => Char - 1
                               }
            , textDocument => #{ uri  => Uri }
            , context      => #{ triggerKind      => TriggerKind
                               , triggerCharacter => TriggerCharacter
                               }
            },
  Content = erlang_ls_protocol:request(RequestId, Method, Params),
  gen_tcp:send(Socket, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({definition, Uri, Line, Char}, From, State) ->
  #state{ request_id = RequestId
        , socket     = Socket
        } = State,
  Method = <<"textDocument/definition">>,
  TextDocument = #{ uri  => Uri },
  Position = #{ line      => Line - 1
              , character => Char - 1
              },
  Params = #{ position     => Position
            , textDocument => TextDocument
            },
  Content = erlang_ls_protocol:request(RequestId, Method, Params),
  gen_tcp:send(Socket, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({document_symbol, Uri}, From, State) ->
  #state{ request_id = RequestId
        , socket     = Socket
        } = State,
  Method = <<"textDocument/documentSymbol">>,
  TextDocument = #{ uri  => Uri },
  Params = #{ textDocument => TextDocument },
  Content = erlang_ls_protocol:request(RequestId, Method, Params),
  gen_tcp:send(Socket, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({references, Uri, Line, Char}, From, State) ->
  #state{ request_id = RequestId
        , socket     = Socket
        } = State,
  Method = <<"textDocument/references">>,
  Params = #{ position     => #{ line      => Line - 1
                               , character => Char - 1
                               }
            , textDocument => #{ uri  => Uri }
            },
  Content = erlang_ls_protocol:request(RequestId, Method, Params),
  gen_tcp:send(Socket, Content),
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
  Content = erlang_ls_protocol:notification(Method, Params),
  ok = gen_tcp:send(State#state.socket, Content),
  {reply, ok, State};
handle_call({did_save, Uri}, _From, State) ->
  Method = <<"textDocument/didSave">>,
  TextDocument = #{ uri => Uri },
  Params = #{textDocument => TextDocument},
  Content = erlang_ls_protocol:notification(Method, Params),
  ok = gen_tcp:send(State#state.socket, Content),
  {reply, ok, State};
handle_call({did_close, Uri}, _From, State) ->
  Method = <<"textDocument/didClose">>,
  TextDocument = #{ uri => Uri },
  Params = #{textDocument => TextDocument},
  Content = erlang_ls_protocol:notification(Method, Params),
  ok = gen_tcp:send(State#state.socket, Content),
  {reply, ok, State};
handle_call({initialize, RootUri, InitOptions}, From, State) ->
  #state{ request_id = RequestId
        , socket     = Socket
        } = State,
  Method  = <<"initialize">>,
  Params  = #{ <<"rootUri">> => RootUri
             , <<"initializationOptions">> => InitOptions
             , <<"capabilities">> =>
                 #{ <<"textDocument">> =>
                      #{ <<"completion">> =>
                           #{ <<"contextSupport">> => 'true' }
                       }
                  }
             },
  Content = erlang_ls_protocol:request(RequestId, Method, Params),
  gen_tcp:send(Socket, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({tcp, _Socket, Packet}, #state{ document  = Document
                                          , socket    = Socket
                                          , pending   = Pending
                                          } = State) ->
  lager:debug("[SERVER] TCP Packet [document=~p] [packet=~p] ", [Document, Packet]),
  Data = <<Document/binary, Packet/binary>>,
  {Responses, NewDocument} = erlang_ls_jsonrpc:split(Data, [return_maps, {labels, atom}]),
  Pending1 = handle_responses(Socket, Responses, Pending),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state{ document = NewDocument, pending = Pending1 }};
handle_info({tcp_closed, _Socket}, State) ->
  lager:debug("[CLIENT] TCP closed", []),
  {noreply, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
  lager:debug("[CLIENT] TCP error [reason=~p]", [Reason]),
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{socket = Socket} = _State) ->
  ok = gen_tcp:close(Socket),
  ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_responses(any(), [map()], [any()]) -> [any()].
handle_responses(_Socket, [], Pending) ->
  Pending;
handle_responses(Socket, [Response|Responses], Pending) ->
  case maps:is_key(id, Response) of
    true ->
      lager:debug("[CLIENT] Handling Response [response=~p]", [Response]),
      RequestId         = maps:get(id, Response),
      {RequestId, From} = lists:keyfind(RequestId, 1, Pending),
      gen_server:reply(From, Response),
      handle_responses(Socket, Responses, lists:keydelete(RequestId, 1, Pending));
    false ->
      lager:debug("[CLIENT] Handling Notification [notification=~p]", [Response]),
      handle_responses(Socket, Responses, Pending)
  end.
