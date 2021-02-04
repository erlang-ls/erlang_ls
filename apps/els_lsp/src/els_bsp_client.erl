%%==============================================================================
%% A client for the Build Server Protocol using the STDIO transport
%%==============================================================================
%% https://build-server-protocol.github.io/docs/specification.html
%%==============================================================================

-module(els_bsp_client).

%%==============================================================================
%% Behaviours
%%==============================================================================

-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ start_link/0
        , start_server/1
        , stop/0
        , request/1
        , request/2
        , notification/1
        , notification/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { request_id = 1 :: request_id()
               , pending    = [] :: [pending_request()]
               , port       :: port() | 'undefined'
               , buffer     = <<>> :: binary()
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()           :: #state{}.
-type request_id()      :: pos_integer().
-type params()          :: #{}.
-type method()          :: binary().
-type pending_request() :: [{request_id(), from()}].
-type from()            :: {pid(), any()}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_server(uri()) -> ok.
start_server(RootUri) ->
  gen_server:call(?SERVER, {start_server, RootUri}).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec notification(method()) -> any().
notification(Method) ->
  notification(Method, #{}).

-spec notification(method(), params()) -> any().
notification(Method, Params) ->
  gen_server:call(?SERVER, {notification, Method, Params}, ?TIMEOUT).

-spec request(method()) -> any().
request(Method) ->
  request(Method, #{}).

-spec request(method(), params()) -> any().
request(Method, Params) ->
  gen_server:call(?SERVER, {request, Method, Params}, ?TIMEOUT).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init([]) -> {ok, state()}.
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

-spec handle_call(any(), any() , state()) ->
        {noreply, state()} | {reply, any(), state()}.
handle_call({start_server, RootUri}, _From, State) ->
  RootPath = binary_to_list(els_uri:path(RootUri)),
  [C|_] = filelib:wildcard(filename:join([RootPath, ".bsp", "*.json"])),
  {ok, Content} = file:read_file(C),
  #{ argv := [Cmd|Params]
   , env := Env0} = jsx:decode(Content, [return_maps, {labels, atom}]),
  Executable = os:find_executable(binary_to_list(Cmd)),
  Args = [binary_to_list(P) || P <- Params],
  Env = [{atom_to_list(K), binary_to_list(V)} || {K, V} <- maps:to_list(Env0)],
  Opts = [{args, Args}, use_stdio, binary, {env, Env}],
  ?LOG_INFO( "Start BSP Server [executable=~p] [args=~p] [env=~p]"
           , [Executable, Args, Env]
           ),
  Port = open_port({spawn_executable, Executable}, Opts),
  {reply, ok, State#state{port = Port}};
handle_call({request, Method, Params}, From, State) ->
  #state{port = Port, request_id = RequestId, pending = Pending} = State,
  ?LOG_INFO( "Sending BSP Request [id=~p] [method=~p] [params=~p]"
           , [RequestId, Method, Params]
           ),
  Payload = els_protocol:request(RequestId, Method, Params),
  port_command(Port, Payload),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | Pending]
                       }};
handle_call({notification, Method, Params}, _From, State) ->
  ?LOG_INFO( "Sending BSP Notification [method=~p] [params=~p]"
           , [Method, Params]
           ),
  #state{port = Port} = State,
  Payload = els_protocol:notification(Method, Params),
  port_command(Port, Payload),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
  NewState = handle_data(Data, State),
  {noreply, NewState};
handle_info(_Request, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{port = Port} = _State) ->
  true = port_close(Port),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_data(binary(), state()) -> state().
handle_data(Data, State) ->
  #state{buffer = Buffer, pending = Pending} = State,
  NewData = <<Buffer/binary, Data/binary>>,
  ?LOG_DEBUG( "Received BSP Data [buffer=~p] [data=~p]"
            , [Buffer, Data]
            ),
  {Messages, NewBuffer} = els_jsonrpc:split(NewData),
  NewPending = lists:foldl(fun handle_message/2, Pending, Messages),
  State#state{buffer = NewBuffer, pending = NewPending}.

-spec handle_message(message(), [pending_request()]) -> [pending_request()].
handle_message(#{ id := Id
                , method := Method
                , params := Params
                } = _Request, Pending) ->
  ?LOG_INFO( "Received BSP Request [id=~p] [method=~p] [params=~p]"
           , [Id, Method, Params]
           ),
  %% TODO: Handle server-initiated request
  Pending;
handle_message(#{id := Id} = Response, Pending) ->
  From = proplists:get_value(Id, Pending),
  gen_server:reply(From, Response),
  lists:keydelete(Id, 1, Pending);
handle_message(#{ method := Method, params := Params}, State) ->
  ?LOG_INFO( "Received BSP Notification [method=~p] [params=~p]"
           , [Method, Params]
           ),
  %% TODO: Handle server-initiated notification
  State.
