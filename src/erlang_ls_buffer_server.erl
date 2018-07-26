%%==============================================================================
%% The Buffer Server
%%==============================================================================
-module(erlang_ls_buffer_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================
%% API
-export([ start_link/0
        , add_buffer/2
        , get_buffer/1
        , remove_buffer/1
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
-record(state, { buffers }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()  :: #state{}.
-type buffer() :: pid().

%%%=============================================================================
%%% API
%%%=============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec add_buffer(uri(), buffer()) -> ok.
add_buffer(Uri, Buffer) ->
  gen_server:call(?SERVER, {add_buffer, Uri, Buffer}).

-spec get_buffer(uri()) -> {ok, buffer()}.
get_buffer(Uri) ->
  gen_server:call(?SERVER, {get_buffer, Uri}).

-spec remove_buffer(uri()) -> ok.
remove_buffer(Uri) ->
  gen_server:call(?SERVER, {remove_buffer, Uri}).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #state{buffers = []}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({add_buffer, Uri, Buffer}, _From, State) ->
  {reply, ok, State#state{ buffers = [{Uri, Buffer}|State#state.buffers]}};
handle_call({get_buffer, Uri}, _From, State) ->
  Buffer = proplists:get_value(Uri, State#state.buffers),
  {reply, {ok, Buffer}, State};
handle_call({remove_buffer, Uri}, _From, State) ->
  Buffers = proplists:delete(Uri, State#state.buffers),
  {reply, ok, State#state{buffers = Buffers}}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
