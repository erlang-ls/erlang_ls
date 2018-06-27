-module(erlang_ls_client).

-behaviour(gen_server).

%% API
-export([ initialize/0
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

-record(state, { socket
               , request_id = 1
               }).

-type state()    :: #state{}.
-type hostname() :: tuple().
-type port_no()  :: pos_integer().

%%%===================================================================
%%% API
%%%===================================================================

-spec initialize() -> ok.
initialize() ->
  gen_server:call(?SERVER, {initialize}).

-spec start_link(hostname(), port_no()) -> {ok, pid()}.
start_link(Host, Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Host, Port}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec init({hostname(), port_no()}) -> {ok, state()}.
init({Host, Port}) ->
  Opts         = [binary, {active, false}],
  {ok, Socket} = gen_tcp:connect(Host, Port, Opts),
  {ok, #state{socket = Socket}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({initialize}, _From, #state{ request_id = RequestId
                                       , socket     = Socket
                                       } = State) ->
  Method = <<"initialize">>,
  Params = #{},
  {ok, Response} = erlang_ls_protocol:request( Socket
                                             , RequestId
                                             , Method
                                             , Params
                                             ),
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
