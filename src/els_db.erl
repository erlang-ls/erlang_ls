-module(els_db).

%% API
-export([ clear_table/1
        , clear_tables/0
        , delete/2
        , delete_object/1
        , install/2
        , lookup/2
        , match/1
        , stop/0
        , transaction/1
        , write/1
        ]).

-define(SERVER, ?MODULE).
-define(TABLES, [ els_dt_document
                , els_dt_document_index
                , els_dt_references
                , els_dt_signatures
                ]).
-define(TIMEOUT, infinity).
-define(DB_SCHEMA_VSN, <<"2">>).
-define(DB_SCHEMA_VSN_FILE, "DB_SCHEMA_VSN").

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
  lager:info("Configuring DB [dir=~s]", [DbDir]),
  ok = filelib:ensure_dir(filename:join([DbDir, "dummy"])),
  ok = application:set_env(mnesia, dir, DbDir),
  %% Avoid mnesia overload while indexing
  ok = application:set_env(mnesia, dump_log_write_threshold, 50000),
  ensure_db(DbDir).

-spec stop() -> ok | {error, any()}.
stop() ->
  application:stop(mnesia).

-spec ensure_db(string()) -> ok.
ensure_db(DbDir) ->
  maybe_delete_db_schema(DbDir),
  ok = write_db_schema_vsn_file(filename:dirname(DbDir)),
  case mnesia:create_schema([node()]) of
    {error, {_, {already_exists, _}}} ->
      lager:info("DB Schema already exists, skipping"),
      ok;
    ok ->
      lager:info("DB Schema created"),
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
  [els_db_table:ensure(T) || T <- ?TABLES],
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
  [ok = clear_table(T) || T <- ?TABLES],
  ok.

-spec wait_for_tables() -> ok.
wait_for_tables() ->
  wait_for_tables(?TIMEOUT).

-spec wait_for_tables(timeout()) -> ok.
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

-spec maybe_delete_db_schema(string()) -> ok.
maybe_delete_db_schema(DbDir) ->
  case read_db_schema_vsn_file(filename:dirname(DbDir)) of
    ?DB_SCHEMA_VSN ->
      Fmt = "DB Schema up to date [dir=~s] [vsn=~s]",
      Args = [DbDir, ?DB_SCHEMA_VSN],
      lager:info(Fmt, Args);
    Vsn ->
      Fmt = "Deleting DB Schema [dir=~s] [current_vsn=~s] [required_vsn=~s]",
      Args = [DbDir, Vsn, ?DB_SCHEMA_VSN],
      lager:info(Fmt, Args),
      ok = del_dir(DbDir)
  end.

-spec write_db_schema_vsn_file(string()) -> ok.
write_db_schema_vsn_file(BaseDir) ->
  ok = file:write_file(db_schema_vsn_file(BaseDir), ?DB_SCHEMA_VSN).

-spec read_db_schema_vsn_file(string()) -> binary().
read_db_schema_vsn_file(BaseDir) ->
  case file:read_file(db_schema_vsn_file(BaseDir)) of
    {ok, Vsn} ->
      Vsn;
    {error, Error} ->
      Fmt = "Error while accessing DB Schema Vsn file [error=~p]",
      Args = [Error],
      lager:warning(Fmt, Args),
      <<>>
  end.

-spec db_schema_vsn_file(string()) -> string().
db_schema_vsn_file(BaseDir) ->
  filename:join([BaseDir, ?DB_SCHEMA_VSN_FILE]).

%% OTP does not include a library function to delete a non-empty dir.
%% This code is adapted from:
%% https://stackoverflow.com/questions/30606773
-spec del_dir(string()) -> ok.
del_dir(Dir) ->
  lists:foreach(fun(D) ->
                    ok = file:del_dir(D)
                end, del_all_files([Dir], [])).

-spec del_all_files([string()], [string()]) -> [string()].
del_all_files([], EmptyDirs) ->
  EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
  {ok, FilesInDir} = file:list_dir(Dir),
  {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                  Path = filename:join([Dir, F]),
                                  case filelib:is_dir(Path) of
                                    true ->
                                      {Fs, [Path | Ds]};
                                    false ->
                                      {[Path | Fs], Ds}
                                  end
                              end, {[], []}, FilesInDir),
  lists:foreach(fun(F) ->
                    ok = file:delete(F)
                end, Files),
  del_all_files(T ++ Dirs, [Dir | EmptyDirs]).
