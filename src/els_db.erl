-module(els_db).

%% API
-export([ clear_table/1
        , clear_tables/0
        , delete/2
        , install/2
        , lookup/2
        , match/1
        , transaction/1
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

-spec install(atom(), string()) -> ok.
install(NodeName, BaseDir) ->
  lager:info("Switching to distribute mode", []),
  ok = start_epmd(),
  net_kernel:start([NodeName, shortnames]),
  lager:info("Distributed mode enabled [node=~p]", [NodeName]),
  DbDir = filename:join([BaseDir, atom_to_list(NodeName)]),
  lager:info("Creating DB [dir=~s]", [DbDir]),
  ok = filelib:ensure_dir(filename:join([DbDir, "dummy"])),
  ok = application:set_env(mnesia, dir, DbDir),
  ensure_db(),
  ok.

-spec ensure_db() -> boolean().
ensure_db() ->
  case mnesia:create_schema([node()]) of
    {error, {_, {already_exists, _}}} ->
      lager:info("DB already exist, skipping"),
      ok;
    ok ->
      ok
  end,
  lager:info("Preparing tables"),
  application:start(mnesia),
  ensure_tables(),
  wait_for_tables(),
  lager:info("DB Initialized"),
  ok.

-spec ensure_tables() -> ok.
ensure_tables() ->
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

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec start_epmd() -> ok.
start_epmd() ->
    [] = os:cmd(epmd_path() ++ " -daemon"),
    ok.

-spec epmd_path() -> string().
epmd_path() ->
    ErtsBinDir = filename:dirname(escript:script_name()),
    Name = "epmd",
    case os:find_executable(Name, ErtsBinDir) of
        false ->
            case os:find_executable(Name) of
                false ->
                    error("Could not find epmd.");
                GlobalEpmd ->
                    GlobalEpmd
            end;
        Epmd ->
            Epmd
    end.
