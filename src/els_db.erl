-module(els_db).

%% API
-export([ find/2
        , find_multi/2
        , keys/1
        , list/1
        , store/3
        , update/4
        , delete/2
        , flush_all_tables/0
        , start_link/0
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

-type state() :: #{}.
-type table() :: atom().
-type key()   :: any().

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

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
