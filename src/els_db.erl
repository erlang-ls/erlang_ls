-module(els_db).

%% API
-export([ add/2
        , find/2
        , find_multi/2
        , install/0
        , install/1
        , keys/1
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
-define(TABLES, [ {documents,  set}
                , {modules,    set}
                , {references, bag}
                , {signatures, set}
                ]).
-define(TIMEOUT, 5000).

-record(poi, { uri   :: erlang_ls_uri:uri()
             , value :: erlang_ls_poi:poi()
             }).

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
  %% TODO: Move to a separate function
  mnesia:create_table( poi
                     , [ {attributes, record_info(fields, poi)}
                       , {disc_copies, []}
                       ]),
  application:stop(mnesia).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% TODO: Rename into store when ready
-spec add(erlang_ls_uri:uri(), erlang_ls_poi:poi()) -> ok.
add(Uri, POI) ->
  F = fun() -> mnesia:write(#poi{uri = Uri, value = POI}) end,
  %% TODO: We probably do not need a transactions per each poi
  mnesia:activity(transaction, F).

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

-spec keys(table()) -> [any()].
keys(Table) ->
  [K || {K, _} <- ets:tab2list(Table)].

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

-spec flush_all_tables() -> ok.
flush_all_tables() ->
  [delete_table(Details) || Details <- ?TABLES],
  [create_table(Details) || Details <- ?TABLES],
  ok.

-spec wait_for_tables() -> ok.
wait_for_tables() ->
  wait_for_tables(?TIMEOUT).

-spec wait_for_tables(pos_integer()) -> ok.
wait_for_tables(Timeout) ->
  %% TODO: Macro for table names
  ok = mnesia:wait_for_tables([poi], Timeout).

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
