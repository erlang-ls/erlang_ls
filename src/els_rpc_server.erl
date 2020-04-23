%%==============================================================================
%% A gen_server for interacting with the runtime node
%% It implements the I/O protocol for capturing standard output.
%%==============================================================================
-module(els_rpc_server).

%%==============================================================================
%% API
%%==============================================================================
-export([ start_link/0 ]).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{acc := [any()]}.

%%==============================================================================
%% External functions
%%==============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init([]) -> {ok, state()}.
init(From) ->
  lager:info("[Group Leader] Starting for ~p", [From]),
  {ok, #{acc => []}}.

-spec handle_call(any(), {pid(), any()}, state()) -> {noreply, state()}.
handle_call(_Request, _From, State) ->
  {noreply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({flush_request, From}, #{acc := Acc} = State) ->
  From ! {flush_response, lists:flatten(lists:reverse(Acc))},
  {noreply, State};
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
