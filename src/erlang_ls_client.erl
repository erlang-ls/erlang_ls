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
-export([ did_open/4
        , did_save/1
        , initialize/0
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
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()    :: #state{}.
-type hostname() :: tuple().
-type port_no()  :: pos_integer().

%%%=============================================================================
%%% API
%%%=============================================================================
-spec did_open(uri(), binary(), number(), binary()) -> ok.
did_open(Uri, LanguageId, Version, Text) ->
  gen_server:call(?SERVER, {did_open, Uri, LanguageId, Version, Text}).

-spec did_save(uri()) -> ok.
did_save(Uri) ->
  gen_server:call(?SERVER, {did_save, Uri}).

-spec initialize() -> ok.
initialize() ->
  gen_server:call(?SERVER, {initialize}).

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
  Opts         = [binary, {active, false}],
  {ok, Socket} = gen_tcp:connect(Host, Port, Opts),
  {ok, #state{socket = Socket}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({did_open, Uri, LanguageId, Version, Text}, _From, State) ->
  Method = <<"textDocument/didOpen">>,
  TextDocument = #{ uri        => Uri
                  , languageId => LanguageId
                  , version    => Version
                  , text       => Text
                  },
  Params = #{textDocument => TextDocument},
  Content = erlang_ls_protocol:notification(Method, Params),
  ok = tcp_send(State#state.socket, Content),
  {reply, ok, State};
handle_call({did_save, Uri}, _From, State) ->
  Method = <<"textDocument/didSave">>,
  TextDocument = #{ uri => Uri },
  Params = #{textDocument => TextDocument},
  Content = erlang_ls_protocol:notification(Method, Params),
  ok = tcp_send(State#state.socket, Content),
  {reply, ok, State};
handle_call({initialize}, _From, #state{ request_id = RequestId
                                       , socket     = Socket
                                       } = State) ->
  Method  = <<"initialize">>,
  Params  = #{},
  Content = erlang_ls_protocol:request(RequestId, Method, Params),
  tcp_send(Socket, Content),
  {ok, Response} = tcp_receive(Socket),
  {reply, Response, State#state{request_id = RequestId + 1}}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
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
-spec tcp_send(gen_tcp:socket(), iodata()) -> ok.
tcp_send(Socket, Content) ->
  ok = gen_tcp:send(Socket, Content).

-spec tcp_receive(gen_tcp:socket()) -> {ok, binary()}.
tcp_receive(Socket) ->
  {ok, Packet}    = gen_tcp:recv(Socket, 0),
  {Headers, Body} = cow_http:parse_headers(Packet),
  BinLength       = proplists:get_value( <<"content-length">>, Headers),
  Length          = binary_to_integer(BinLength),
  tcp_receive(Socket, Body, Length).

-spec tcp_receive(gen_tcp:socket(), binary(), non_neg_integer()) ->
  {ok, binary()}.
tcp_receive(Socket, Body, Length) ->
  case byte_size(Body) < Length of
    true ->
      {ok, Packet} = gen_tcp:recv(Socket, 0),
      tcp_receive(Socket, <<Body/binary, Packet/binary>>, Length);
    false ->
      {ok, jsx:decode(Body, [return_maps, {labels, atom}])}
  end.
