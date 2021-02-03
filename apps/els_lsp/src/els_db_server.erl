%%%=============================================================================
%%% @doc The db gen_server.
%%% @end
%%%=============================================================================

-module(els_db_server).

%%==============================================================================
%% API
%%==============================================================================
-export([ start_link/0
        , clear_table/1
        , delete/2
        , delete_object/2
        , match_delete/2
        , write/2
        ]).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).
-type state() :: #{}.

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, unused, []).

-spec clear_table(atom()) -> ok.
clear_table(Table) ->
  gen_server:call(?SERVER, {clear_table, Table}).

-spec delete(atom(), any()) -> ok.
delete(Table, Key) ->
  gen_server:call(?SERVER, {delete, Table, Key}).

-spec delete_object(atom(), any()) -> ok.
delete_object(Table, Key) ->
  gen_server:call(?SERVER, {delete_object, Table, Key}).

-spec match_delete(atom(), tuple()) -> ok.
match_delete(Table, Pattern) ->
  gen_server:call(?SERVER, {match_delete, Table, Pattern}).

-spec write(atom(), tuple()) -> ok.
write(Table, Object) ->
  gen_server:call(?SERVER, {write, Table, Object}).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-spec init(unused) -> {ok, state()}.
init(unused) ->
  [ok = els_db_table:init(Table) || Table <- els_db:tables()],
  {ok, #{}}.

-spec handle_call(any(), {pid(), any()}, state()) ->
        {reply, any(), state()} | {noreply, state()}.
handle_call({clear_table, Table}, _From, State) ->
  true = ets:delete_all_objects(Table),
  {reply, ok, State};
handle_call({delete, Table, Key}, _From, State) ->
  true = ets:delete(Table, Key),
  {reply, ok, State};
handle_call({delete_object, Table, Key}, _From, State) ->
  true = ets:delete_object(Table, Key),
  {reply, ok, State};
handle_call({match_delete, Table, Pattern}, _From, State) ->
  true = ets:match_delete(Table, Pattern),
  {reply, ok, State};
handle_call({write, Table, Object}, _From, State) ->
  true = ets:insert(Table, Object),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Request, State) ->
  {noreply, State}.
