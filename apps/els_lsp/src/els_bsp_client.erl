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
        , wait_response/2
        , check_response/2
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
-define(BSP_WILDCARD, "*.json").
-define(BSP_CONF_DIR, ".bsp").

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { request_id = 1    :: request_id()
               , pending    = []   :: [pending_request()]
               , port              :: port() | 'undefined'
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
%% Compat Stuff
%% Since the build server can take arbitrary amounts of time to process things
%% we really would like to use gen_server:send_request et co that are added in
%% OTP 23, but we also need to support older OTP versions for now - implement
%% rudimentary scaffolding to fake the new functionality.
%% Remove whenever only OTP >= 23 is supported.
%%==============================================================================
-type server_ref() :: atom() | pid().

-if(?OTP_RELEASE >= 23).
-spec do_send_request(server_ref(), any()) -> any().
do_send_request(ServerRef, Request) ->
  gen_server:send_request(ServerRef, Request).

-spec do_wait_response(any(), timeout()) ->
        {reply, any()} |
        timeout |
        {error, {any(), server_ref()}}.
do_wait_response(RequestId, Timeout) ->
  gen_server:wait_response(RequestId, Timeout).

-spec do_check_response(any(), any()) ->
        {reply, any()} |
        no_reply |
        {error, {any(), server_ref()}}.
do_check_response(Msg, RequestId) ->
  gen_server:check_response(Msg, RequestId).
-else.
-spec do_send_request(server_ref(), any()) -> any().
do_send_request(ServerRef, Request) ->
  Self = self(),
  Ref = erlang:make_ref(),
  F = fun() ->
          Result = gen_server:call(ServerRef, Request, infinity),
          try Self ! {Ref, Result} catch _:_ -> ok end
      end,
  {Pid, Mon} = erlang:spawn_monitor(F),
  {Pid, Mon, Ref, ServerRef}.

-spec do_wait_response(any(), timeout()) ->
        {reply, any()} |
        timeout |
        {error, {any(), server_ref()}}.
do_wait_response({_Pid, Mon, Ref, ServerRef}, Timeout) ->
  receive
    {Ref, Result} ->
      erlang:demonitor(Mon, [flush]),
      {reply, Result};
    {'DOWN', Mon, _Type, _Object, Info} ->
      {error, {Info, ServerRef}}
  after Timeout ->
      timeout
  end.

-spec do_check_response(any(), any()) ->
        {reply, any()} |
        no_reply |
        {error, {any(), server_ref()}}.
do_check_response(Msg, {_Pid, Mon, Ref, ServerRef}) ->
  case Msg of
    {Ref, Result} ->
      erlang:demonitor(Mon, [flush]),
      {reply, Result};
    {'DOWN', Mon, _Type, _Object, Info} ->
      {error, {Info, ServerRef}};
    _ ->
      no_reply
  end.
-endif.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_server(uri()) -> {ok, map()} | {error, any()}.
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
  gen_server:cast(?SERVER, {notification, Method, Params}).

-spec request(method()) -> any().
request(Method) ->
  request(Method, #{}).

-spec request(method(), params()) -> any().
request(Method, Params) ->
  do_send_request(?SERVER, {request, Method, Params}).

-spec wait_response(any(), timeout()) ->
        {reply, any()} | timeout | {error, {any(), any()}}.
wait_response(RequestId, Timeout) ->
  do_wait_response(RequestId, Timeout).

-spec check_response(any(), any()) ->
        {reply, any()} | no_reply | {error, {any(), any()}}.
check_response(Msg, RequestId) ->
  do_check_response(Msg, RequestId).

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
  RootPath = els_uri:path(RootUri),
  case find_config(RootPath) of
    undefined ->
      ?LOG_INFO("Found no BSP configuration. [root=~p]", [RootPath]),
      {reply, {error, noconfig}, State};
    #{ argv := [Cmd|Params] } = Config ->
      Executable = os:find_executable(binary_to_list(Cmd)),
      Args = [binary_to_list(P) || P <- Params],
      Opts = [{args, Args}, use_stdio, binary],
      ?LOG_INFO( "Start BSP Server [executable=~p] [args=~p]"
               , [Executable, Args]
               ),
      Port = open_port({spawn_executable, Executable}, Opts),
      {reply, {ok, Config}, State#state{port = Port}}
  end;
handle_call({request, Method, Params}, From, State) ->
  #state{port = Port, request_id = RequestId, pending = Pending} = State,
  ?LOG_INFO( "Sending BSP Request [id=~p] [method=~p] [params=~p]"
           , [RequestId, Method, Params]
           ),
  Payload = els_protocol:request(RequestId, Method, Params),
  port_command(Port, Payload),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | Pending]
                       }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({notification, Method, Params}, State) ->
  ?LOG_INFO( "Sending BSP Notification [method=~p] [params=~p]"
           , [Method, Params]
           ),
  #state{port = Port} = State,
  Payload = els_protocol:notification(Method, Params),
  port_command(Port, Payload),
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
  NewState = handle_data(Data, State),
  {noreply, NewState};
handle_info(_Request, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{port = Port} = _State) ->
  case Port of
    undefined ->
      ok;
    _ ->
      port_close(Port)
  end,
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec find_config(els_uri:path()) -> map() | undefined.
find_config(RootDir) ->
  Wildcard = filename:join([RootDir, ?BSP_CONF_DIR, ?BSP_WILDCARD]),
  Candidates = filelib:wildcard(els_utils:to_list(Wildcard)),
  choose_config(Candidates).

-spec choose_config([file:filename()]) -> map() | undefined.
choose_config([]) ->
  undefined;
choose_config([F|Fs]) ->
  try
    {ok, Content} = file:read_file(F),
    Config = jsx:decode(Content, [return_maps, {labels, atom}]),
    Languages = maps:get(languages, Config),
    case lists:member(<<"erlang">>, Languages) of
      true ->
        Config;
      false ->
        choose_config(Fs)
    end
  catch
    C:E:S ->
      ?LOG_ERROR( "Bad BSP config file. [file=~p] [error=~p]"
                , [F, {C, E, S}]
                ),
      choose_config(Fs)
  end.

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
