-module(els_db).

%% API
-export([ clear_table/1
        , clear_tables/0
        , delete/2
        , delete_object/1
        , install/0
        , lookup/2
        , match/1
        , stop/0
        , tables/0
        , transaction/1
        , write/1
        ]).

-define(SERVER, ?MODULE).

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec install() -> ok.
install() ->
  lager:info("Configuring DB", []),
  ok = application:set_env(mnesia, schema_location, ram),
  %% Avoid mnesia overload while indexing
  ok = application:set_env(mnesia, dump_log_write_threshold, 50000),
  %% We have 4 tables. Let's load them in parallel
  ok = application:set_env(mnesia, no_table_loaders, 4),
  ensure_db().

-spec stop() -> ok | {error, any()}.
stop() ->
  application:stop(mnesia).

-spec ensure_db() -> ok.
ensure_db() ->
  lager:info("Preparing tables"),
  application:start(mnesia),
  ensure_tables(),
  lager:info("DB Initialized"),
  ok.

-spec ensure_tables() -> ok.
ensure_tables() ->
  [els_db_table:ensure(T) || T <- tables()],
  ok.

-spec delete(atom(), any()) -> ok.
delete(Table, Key) ->
  mnesia:dirty_delete({Table, Key}).

-spec delete_object(any()) -> ok.
delete_object(Item) ->
  mnesia:dirty_delete_object(Item).

-spec lookup(atom(), any()) -> {ok, [tuple()]}.
lookup(Table, Key) ->
  {ok, mnesia:dirty_read(Table, Key)}.

-spec match(tuple()) -> {ok, [tuple()]}.
match(Pattern) when is_tuple(Pattern) ->
  {ok, mnesia:dirty_match_object(Pattern)}.

-spec write(tuple()) -> ok.
write(Record) when is_tuple(Record) ->
  mnesia:dirty_write(Record).

-spec clear_tables() -> ok.
clear_tables() ->
  [ok = clear_table(T) || T <- tables()],
  ok.

-spec clear_table(atom()) -> ok.
clear_table(Table) ->
  mnesia:clear_table(Table),
  ok.

-spec tables() -> [atom()].
tables() ->
  [ els_dt_document
  , els_dt_document_index
  , els_dt_references
  , els_dt_signatures
  ].

-spec transaction(function()) -> ok | {ok, any()} | {error, any()}.
transaction(F) ->
  case mnesia:transaction(F) of
    {aborted, Reason} -> {error, {aborted, Reason}};
    {atomic, ok}      -> ok;
    {atomic, Result}  -> {ok, Result}
  end.
