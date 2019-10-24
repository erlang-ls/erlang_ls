-module(erlang_ls_db).

%% API
-export([ find/2
        , list/1
        , store/3
        , delete/2
        , start_link/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-define(SERVER, ?MODULE).
%% TODO: Get table names via provider callbacks
-define(TABLES, [ completion_index
                , documents
                , references_index
                ]).

-type state() :: #{}.
-type table() :: atom().
-type key()   :: any().

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec find(table(), key()) -> {ok, any()} | not_found.
find(Table, Key) ->
  case ets:lookup(Table, Key) of
    [] -> not_found;
    [{Key, Value}] -> {ok, Value}
  end.

-spec list(table()) -> [any()].
list(Table) ->
  ets:tab2list(Table).

-spec store(table(), key(), any()) -> ok.
store(Table, Key, Value) ->
  true = ets:insert(Table, {Key, Value}),
  ok.

-spec delete(table(), key()) -> ok.
delete(Table, Key) ->
  true = ets:delete(Table, Key),
  ok.

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================

-spec init({}) -> {ok, state()}.
init({}) ->
  [create_table(Name) || Name <- ?TABLES],
  {ok, #{}}.

-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec create_table(atom()) -> ok.
create_table(Name) ->
  ets:new(Name, [named_table, set, public]),
  ok.
