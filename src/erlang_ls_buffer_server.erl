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
        , get_deps_dirs/0
        , get_otp_path/0
        , get_root_uri/0
        , remove_buffer/1
        , set_deps_dirs/1
        , set_otp_path/1
        , set_root_uri/1
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
-record(state, { buffers
               , deps_dirs = []
               , root_uri
               , otp_path
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()  :: #state{}.
-type buffer() :: pid().
-type path()   :: file:filename().

%%%=============================================================================
%%% API
%%%=============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec add_buffer(erlang_ls_uri:uri(), buffer()) -> ok.
add_buffer(Uri, Buffer) ->
  gen_server:call(?SERVER, {add_buffer, Uri, Buffer}).

-spec get_buffer(erlang_ls_uri:uri()) -> {ok, buffer() | undefined}.
get_buffer(Uri) ->
  gen_server:call(?SERVER, {get_buffer, Uri}).

-spec get_deps_dirs() -> {ok, [path()]}.
get_deps_dirs() ->
  gen_server:call(?SERVER, {get_deps_dirs}).

-spec get_otp_path() -> {ok, path() | undefined}.
get_otp_path() ->
  gen_server:call(?SERVER, {get_otp_path}).

-spec get_root_uri() -> {ok, erlang_ls_uri:uri() | undefined}.
get_root_uri() ->
  gen_server:call(?SERVER, {get_root_uri}).

-spec remove_buffer(erlang_ls_uri:uri()) -> ok.
remove_buffer(Uri) ->
  gen_server:call(?SERVER, {remove_buffer, Uri}).

-spec set_deps_dirs([erlang_ls_uri:path()]) -> ok.
set_deps_dirs(DepsDirs) ->
  gen_server:call(?SERVER, {set_deps_dirs, DepsDirs}).

-spec set_otp_path(erlang_ls_uri:uri()) -> ok.
set_otp_path(Uri) ->
  gen_server:call(?SERVER, {set_otp_path, Uri}).

-spec set_root_uri(erlang_ls_uri:uri()) -> ok.
set_root_uri(Uri) ->
  gen_server:call(?SERVER, {set_root_uri, Uri}).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #state{buffers = []}}.

-spec handle_call(any(), any(), state()) -> {reply, ok | {ok, buffer()}, state()}.
handle_call({add_buffer, Uri, Buffer}, _From, State) ->
  {reply, ok, State#state{ buffers = [{Uri, Buffer}|State#state.buffers]}};
handle_call({get_buffer, Uri}, _From, State) ->
  Buffer = proplists:get_value(Uri, State#state.buffers),
  {reply, {ok, Buffer}, State};
handle_call({get_deps_dirs}, _From, State) ->
  DepsDirs = State#state.deps_dirs,
  {reply, {ok, DepsDirs}, State};
handle_call({get_otp_path}, _From, State) ->
  OtpPath = State#state.otp_path,
  {reply, {ok, OtpPath}, State};
handle_call({get_root_uri}, _From, State) ->
  RootUri = State#state.root_uri,
  {reply, {ok, RootUri}, State};
handle_call({remove_buffer, Uri}, _From, State) ->
  Buffers = proplists:delete(Uri, State#state.buffers),
  {reply, ok, State#state{buffers = Buffers}};
handle_call({set_deps_dirs, DepsDirs}, _From, State) ->
  {reply, ok, State#state{deps_dirs = DepsDirs}};
handle_call({set_otp_path, Path}, _From, State) ->
  {reply, ok, State#state{otp_path = Path}};
handle_call({set_root_uri, Uri}, _From, State) ->
  {reply, ok, State#state{root_uri = Uri}}.

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
