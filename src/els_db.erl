%% TODO: No need for a gen_server
-module(els_db).

%% Experimental API
-export([ lookup/2
        , match/1
        , write/1
        ]).

%% API
-export([ find/2
        , find_multi/2
        , install/0
        , install/1
        , list/1
        , store/3
        , update/4
        , delete/2
        , flush_all_tables/0
        , start_link/0
        , wait_for_tables/0
        , wait_for_tables/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-define(SERVER, ?MODULE).
-define(TABLES, [ {references, bag}
                ]).
%% TODO: Merge with TABLES
-define(DB_TABLES, [ els_dt_module
                   , els_dt_documents
                   , els_dt_signatures
                   ]).
-define(TIMEOUT, 5000).

-type state() :: #{}.
-type table() :: atom().
-type key()   :: any().

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec install() -> ok.
install() ->
  {ok, Dir} = application:get_env(erlang_ls, db_dir),
  install(Dir).

-spec install(string()) -> ok.
install(Dir) ->
  lager:info("Creating DB. [dir=~s]", [Dir]),
  ok = filelib:ensure_dir(filename:join([Dir, "dummy"])),
  ok = application:set_env(mnesia, dir, Dir),
  mnesia:create_schema([node()]),
  application:start(mnesia),
  create_tables(),
  application:stop(mnesia).

-spec create_tables() -> ok.
create_tables() ->
  [els_db_table:create(T) || T <- ?DB_TABLES],
  ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec lookup(atom(), any()) -> {ok, [tuple()]} | {error, any()}.
lookup(Table, Key) ->
  F = fun() -> mnesia:read({Table, Key}) end,
  case mnesia:transaction(F) of
    {atomic, Items}   -> {ok, Items};
    {aborted, Reason} -> {error, Reason}
  end.

-spec match(tuple()) -> {ok, [tuple()]} | {error, any()}.
match(Pattern) when is_tuple(Pattern) ->
  F = fun() -> mnesia:match_object(Pattern) end,
  case mnesia:transaction(F) of
    {atomic, Items}   -> {ok, Items};
    {aborted, Reason} -> {error, Reason}
  end.

-spec write(tuple()) -> ok | {error, any()}.
write(Record) when is_tuple(Record) ->
  F = fun() -> mnesia:write(Record) end,
  case mnesia:transaction(F) of
    {aborted, Reason} -> {error, {aborted, Reason}};
    {atomic, ok}      -> ok;
    {atomic, Result}  -> {error, Result}
  end.

-spec find(table(), key()) -> {ok, any()} | {error, not_found}.
find(Table, Key) ->
  case ets:lookup(Table, Key) of
    [] -> {error, not_found};
    [{Key, Value}] -> {ok, Value}
  end.

-spec find_multi(table(), key()) -> {ok, nonempty_list(any())}
                                    | {error, not_found}.
find_multi(Table, Key) ->
  case ets:lookup(Table, Key) of
    [] -> {error, not_found};
    KVs -> {ok, KVs} %% Return as is, we need to iterate the result
                     %% anyway, no point doing it twice.
  end.

-spec list(table()) -> [any()].
list(Table) ->
  ets:tab2list(Table).

-spec store(table(), key(), any()) -> ok.
store(Table, Key, Value) ->
  ets:insert(Table, {Key, Value}),
  ok.

-spec update(table(), key(), function(), any()) -> ok.
update(Table, Key, UpdateFun, Default) ->
  case find(Table, Key) of
    {error, not_found} ->
      New = UpdateFun(Default),
      case ets:insert_new(Table, {Key, New}) of
        true  -> ok;
        false -> update(Table, Key, UpdateFun, Default)
      end;
    {ok, Old} ->
      New = UpdateFun(Old),
      Replace = [{{Key, Old}, [], [{const, {Key, New}}]}],
      case ets:select_replace(Table, Replace) of
        1 -> ok;
        %% AZ: The next line is a recursive call. If it is ever hit,
        %%     will we have an infinite loop?
        0 -> update(Table, Key, UpdateFun, Default)
      end
  end.

-spec delete(table(), key()) -> ok.
delete(Table, Key) ->
  true = ets:delete(Table, Key),
  ok.

%% TODO: Merge
-spec flush_all_tables() -> ok.
flush_all_tables() ->
  [delete_table(Details) || Details <- ?TABLES],
  [clear_table(T) || T <- ?DB_TABLES],
  [create_table(Details) || Details <- ?TABLES],
  ok.

-spec wait_for_tables() -> ok.
wait_for_tables() ->
  wait_for_tables(?TIMEOUT).

-spec wait_for_tables(pos_integer()) -> ok.
wait_for_tables(Timeout) ->
  ok = mnesia:wait_for_tables(?DB_TABLES, Timeout).

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

-spec create_table({atom(), atom()}) -> ok.
create_table({Name, Type}) ->
  Opts = [ named_table
         , Type
         , public
         , {write_concurrency, true}
         , compressed
         ],
  ets:new(Name, Opts),
  ok.

-spec delete_table({atom(), atom()}) -> ok.
delete_table({Name, _}) ->
  ets:delete(Name),
  ok.

%% TODO: Merge with above
-spec clear_table(atom()) -> ok.
clear_table(Table) ->
  mnesia:clear_table(Table),
  ok.
