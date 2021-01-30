%%==============================================================================
%% A client for the Build Server Protocol using the STDIO transport
%%==============================================================================
-module(els_bsp_client).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).
%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%==============================================================================
%% Exports
%%==============================================================================
%% Erlang API
-export([ start_link/1
        , start_link/3
        , stop/0
        ]).

%% Server Lifetime
-export([ build_initialize/1
        , build_initialized/1
        , build_shutdown/0
        , build_exit/0
        , build_show_message/1
        , build_log_message/1
        , build_publish_diagnostics/1
        ]).

%% Custom
-export([ custom_format/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, infinity).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { request_id    = 1 :: request_id()
               , pending       = []
               , notifications = []
               , requests      = []
               , port          :: port()
               , buffer        :: binary()
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()      :: #state{}.
-type request_id() :: pos_integer().
-type params()     :: #{}.

%%==============================================================================
%% Erlang API
%%==============================================================================
-spec start_link(string()) -> {ok, pid()}.
start_link(RootPath) ->
  {ok, Executable, Args, Env} = els_bsp_connection:discover(RootPath),
  start_link(Executable, Args, Env).

-spec start_link(string(), [string()], [#{string() := string()}]) ->
        {ok, pid()}.
start_link(Executable, Args, Env) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Executable, Args, Env}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

%%==============================================================================
%% Server Lifetime
%%==============================================================================
-spec build_initialize(params()) -> map().
build_initialize(Params) ->
  gen_server:call(?SERVER, {build_initialize, Params}, ?TIMEOUT).

-spec build_initialized(params()) -> map().
build_initialized(Params) ->
  gen_server:call(?SERVER, {build_initialized, Params}).

-spec build_shutdown() -> map().
build_shutdown() ->
  gen_server:call(?SERVER, {shutdown}).

-spec build_exit() -> ok.
build_exit() ->
  gen_server:call(?SERVER, {exit}).

-spec build_show_message(params()) -> ok.
build_show_message(Params) ->
  gen_server:call(?SERVER, {build_show_message, Params}).

-spec build_log_message(params()) -> ok.
build_log_message(Params) ->
  gen_server:call(?SERVER, {build_log_message, Params}).

-spec build_publish_diagnostics(params()) -> ok.
build_publish_diagnostics(Params) ->
  gen_server:call(?SERVER, {build_publish_diagnostics, Params}).

-spec custom_format(params()) -> ok.
custom_format(Params) ->
  gen_server:call(?SERVER, {custom_format, Params}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({string(), [string()], [{string(), string()}]}) -> {ok, state()}.
init({Executable, Args, Env}) ->
  process_flag(trap_exit, true),
  Opts = [{args, Args}, use_stdio, binary, {env, Env}],
  Port = open_port({spawn_executable, Executable}, Opts),
  {ok, #state{port = Port, buffer = <<>>}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({build_initialized, Opts}, _From, #state{port = Port} = State) ->
  Method = method_lookup(build_initialized),
  Params = notification_params(Opts),
  Content = els_protocol:notification(Method, Params),
  send(Port, Content),
  {reply, ok, State};
handle_call({exit}, _From, #state{port = Port} = State) ->
  RequestId = State#state.request_id,
  Method = <<"exit">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(Port, Content),
  {reply, ok, State};
handle_call({shutdown}, From, #state{port = Port} = State) ->
  RequestId = State#state.request_id,
  Method = <<"shutdown">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  send(Port, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call(Input = {build_initialize, _}, From, State) ->
  #state{ port = Port, request_id = RequestId } = State,
  Method = method_lookup(build_initialize),
  Params = request_params(Input),
  Content = els_protocol:request(RequestId, Method, Params),
  send(Port, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({custom_format, Params}, From, State) ->
  #state{ port = Port, request_id = RequestId } = State,
  Method = <<"custom/format">>,
  Content = els_protocol:request(RequestId, Method, Params),
  send(Port, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call(Request, _From, State) ->
  {reply, {error, {unexpected_request, Request}}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
  #state{ pending = Pending0
        , notifications = Notifications0
        , requests = Requests0
        , buffer = Buffer
        } = State,
  NewData = <<Buffer/binary, Data/binary>>,
  {Responses, NewBuffer} = els_jsonrpc:split(NewData),
  {Pending, Notifications, Requests}
    = do_handle_messages(Responses, Pending0, Notifications0, Requests0),
  {noreply, State#state{ pending = Pending
                       , notifications = Notifications
                       , requests = Requests
                       , buffer = NewBuffer
                       }};
handle_info(_Request, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{port = Port} = _State) ->
  true = port_close(Port),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec do_handle_messages([map()], [any()], [any()], [any()]) ->
        {[any()], [any()], [any()]}.
do_handle_messages([], Pending, Notifications, Requests) ->
  {Pending, Notifications, Requests};
do_handle_messages([Message|Messages], Pending, Notifications, Requests) ->
  case is_response(Message) of
    true ->
      RequestId = maps:get(id, Message),
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
          do_handle_messages( Messages
                            , Pending
                            , [Message|Notifications]
                            , Requests);
        false ->
          do_handle_messages( Messages
                            , Pending
                            , Notifications
                            , [Message|Requests])
      end
  end.

-spec request_params(tuple()) -> any().
request_params({build_initialize, RootUri}) ->
  {ok, Vsn} = application:get_key(erlang_ls, vsn),
  #{ <<"displayName">>  => <<"Erlang LS BSP Client">>
   , <<"version">>      => list_to_binary(Vsn)
   , <<"bspVersion">>   => <<"2.0.0">>
   , <<"rootUri">>      => RootUri
   , <<"capabilities">> => #{ <<"languageIds">> => [<<"erlang">>] }
   , <<"data">>         => #{}
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
notification_params(_) ->
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

-spec send(port(), binary()) -> ok.
send(Port, Payload) ->
  port_command(Port, Payload).

-spec method_lookup(atom()) -> binary().
method_lookup(build_initialize) -> <<"build/initialize">>;
method_lookup(build_initialized) -> <<"build/initialized">>.
