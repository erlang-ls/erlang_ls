%%==============================================================================
%% Erlang LS TCP Client
%%==============================================================================

-module(els_tcp_client).

%%==============================================================================
%% Behaviour els_client
%%==============================================================================

-behaviour(els_client).
-export( [ start_link/1
         , send/2
         ]).

%%==============================================================================
%% Behaviour gen_server
%%==============================================================================

-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type args()  :: #{ host := tuple()
                  , port := pos_integer()
                  }.
-type state() :: #{ socket := any()
                  , buffer := binary()
                  }.

%%==============================================================================
%% Callbacks for the els_client behaviour
%%==============================================================================

-spec start_link(args()) -> {ok, pid()}.
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

-spec init(args()) -> {ok, state()}.
init(#{host := Host, port := Port}) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, default_tcp_opts()),
  {ok, #{socket => Socket, buffer => <<>>}}.

-spec send(pid(), binary()) -> ok.
send(Server, Payload) ->
  gen_server:call(Server, {send, Payload}).

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #{socket := Socket}) ->
  ok = gen_tcp:close(Socket).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================

-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call({send, Payload}, _From, #{ socket := Socket } = State) ->
  ok = gen_tcp:send(Socket, Payload),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({tcp, Socket, Packet}, State) ->
  #{socket := Socket, buffer := Buffer0} = State,
  Data = <<Buffer0/binary, Packet/binary>>,
  {Responses, Buffer} = els_jsonrpc:split(Data),
  els_client:handle_responses(Responses),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#{ buffer => Buffer }};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
  {stop, Reason, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec default_tcp_opts() -> [any()].
default_tcp_opts() ->
  [binary, {active, once}, {packet, 0}].
