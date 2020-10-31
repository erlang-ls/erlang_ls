%%==============================================================================
%% The 'els_db_table' behaviour
%%==============================================================================

-module(els_db_table).

%%==============================================================================
%% Callbacks
%%==============================================================================

-callback name() -> atom().
-callback opts() -> proplists:proplist().

%%==============================================================================
%% Exports
%%==============================================================================

-export([ default_opts/0
        , init/1
        , name/1
        , opts/1
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type table() :: atom().
-export_type([ table/0 ]).

%%==============================================================================
%% API
%%==============================================================================

-spec default_opts() -> [any()].
default_opts() ->
  [public, named_table, {keypos, 2}, {read_concurrency, true}].

-spec init(table()) -> ok.
init(Table) ->
  TableName = name(Table),
  lager:info("Creating table [name=~p]", [TableName]),
  ets:new(TableName, opts(Table)),
  ok.

-spec name(table()) -> atom().
name(Table) ->
  Table:name().

-spec opts(table()) -> proplists:proplist().
opts(Table) ->
  default_opts() ++ Table:opts().
