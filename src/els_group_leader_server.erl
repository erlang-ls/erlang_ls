%%==============================================================================
%% A gen_server implementing the Erlang I/O protocol.
%% Used for capturing standard output during RPC calls towards the runtime node.
%%==============================================================================
-module(els_group_leader_server).

%%==============================================================================
%% API
%%==============================================================================
-export([ new/0
        , flush/1
        , stop/1
        ]).

%%==============================================================================
%% Supervisor API
%%==============================================================================
-export([ start_link/1 ]).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type config() :: #{ caller := pid()
                   , gl := pid()
                   }.
-type state() :: #{ acc := [any()]
                  , caller := pid()
                  , gl := pid()
                  }.

%%==============================================================================
%% API
%%==============================================================================

%% @doc Create a new server
-spec new() -> {ok, pid()}.
new() ->
  Caller = self(),
  GL = group_leader(),
  supervisor:start_child(els_group_leader_sup, [#{caller => Caller, gl => GL}]).

-spec flush(pid()) -> binary().
flush(Server) ->
  gen_server:call(Server, {flush}).

-spec stop(pid()) -> ok.
stop(Server) ->
  gen_server:call(Server, {stop}).

%%==============================================================================
%% Supervisor API
%%==============================================================================
-spec start_link(config()) -> {ok, pid()}.
start_link(Config) ->
  gen_server:start_link(?MODULE, Config, []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(config()) -> {ok, state()}.
init(#{caller := Caller, gl := GL}) ->
  process_flag(trap_exit, true),
  lager:info("Starting group leader server [caller=~p] [gl=~p]", [Caller, GL]),
  group_leader(self(), Caller),
  {ok, #{acc => [], caller => Caller, gl => GL}}.

-spec handle_call(any(), {pid(), any()}, state()) -> {noreply, state()}.
handle_call({flush}, _From, #{acc := Acc} = State) ->
  Res = els_utils:to_binary(lists:flatten(lists:reverse(Acc))),
  {reply, Res, State};
handle_call({stop}, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({io_request, From, ReplyAs, Request}, State) ->
  #{acc := Acc} = State,
  case Request of
    {put_chars, Encoding, Characters} ->
      List = unicode:characters_to_list(Characters, Encoding),
      From ! {io_reply, ReplyAs, ok},
      {noreply, State#{acc => [List|Acc]}};
    {put_chars, Encoding, M, F, A} ->
      String = try erlang:apply(M, F, A)
               catch
                 C:E:S ->
                   io_lib:format("~p", [{C, E, S}])
               end,
      List = unicode:characters_to_list(String, Encoding),
      From ! {io_reply, ReplyAs, ok},
      {noreply, State#{acc => [List|Acc]}};
    Else ->
      lager:warning("[Group Leader] Request not implemented", [Else]),
      From ! {io_reply, ReplyAs, {error, not_implemented}},
      {noreply, State}
  end;
handle_info(Request, State) ->
  lager:warning("[Group Leader] Unexpected request", [Request]),
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #{caller := Caller, gl := GL} = _State) ->
  group_leader(GL, Caller).
