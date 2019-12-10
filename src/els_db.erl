-module(els_db).

%% API
-export([ clear_table/1
        , clear_tables/0
        , delete/2
        , install/0
        , install/1
        , lookup/2
        , match/1
        , transaction/1
        , wait_for_tables/0
        , wait_for_tables/1
        , write/1
        ]).

-define(SERVER, ?MODULE).
-define(TABLES, [ els_dt_document
                , els_dt_references
                , els_dt_signatures
                ]).
-define(TIMEOUT, 5000).

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
  [els_db_table:create(T) || T <- ?TABLES],
  ok.

-spec delete(atom(), any()) -> ok | {error, any()}.
delete(Table, Key) ->
  F = fun() -> mnesia:delete({Table, Key}) end,
  transaction(F).

-spec lookup(atom(), any()) -> {ok, [tuple()]} | {error, any()}.
lookup(Table, Key) ->
  F = fun() -> mnesia:read({Table, Key}) end,
  transaction(F).

-spec match(tuple()) -> {ok, [tuple()]} | {error, any()}.
match(Pattern) when is_tuple(Pattern) ->
  F = fun() -> mnesia:match_object(Pattern) end,
  transaction(F).

-spec write(tuple()) -> ok | {error, any()}.
write(Record) when is_tuple(Record) ->
  F = fun() -> mnesia:write(Record) end,
  transaction(F).

-spec clear_tables() -> ok.
clear_tables() ->
  [ok = clear_table(T) || T <- ?TABLES],
  ok.

-spec wait_for_tables() -> ok.
wait_for_tables() ->
  wait_for_tables(?TIMEOUT).

-spec wait_for_tables(pos_integer()) -> ok.
wait_for_tables(Timeout) ->
  ok = mnesia:wait_for_tables(?TABLES, Timeout).

-spec clear_table(atom()) -> ok.
clear_table(Table) ->
  mnesia:clear_table(Table),
  ok.

-spec transaction(function()) -> ok | {ok, any()} | {error, any()}.
transaction(F) ->
  case mnesia:transaction(F) of
    {aborted, Reason} -> {error, {aborted, Reason}};
    {atomic, ok}      -> ok;
    {atomic, Result}  -> {ok, Result}
  end.
