%%==============================================================================
%% The 'module' table
%%==============================================================================

-module(els_dt_module).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-export([ name/0
        , opts/0
        ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ find_all/0
        , find_by/1
        , find_by_module/1
        , insert/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_module, { module :: atom() | '_'
                       , uri    :: uri()  | '_'
                       }).
-type els_dt_module() :: #els_dt_module{}.

-type item() :: #{ module := atom()
                 , uri    := uri()
                 }.
-export_type([ item/0 ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [ {attributes , record_info(fields, els_dt_module)}
  , {disc_copies, []}
  , {index      , [#els_dt_module.uri]}
  , {type       , set}
  ].

%%==============================================================================
%% API
%%==============================================================================

-spec from_map(item()) -> els_dt_module().
from_map(#{ module := Module, uri := Uri}) ->
  #els_dt_module{ module = Module, uri = Uri}.

-spec to_map(els_dt_module()) -> item().
to_map(#els_dt_module{ module = Module, uri = Uri}) ->
  #{ module => Module
   , uri    => Uri
   }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_map(Map),
  els_db:write(Record).

%% @edoc Find all
-spec find_all() -> {ok, [item()]} | {error, any()}.
find_all() ->
  Pattern = #els_dt_module{_ = '_'},
  find_by(Pattern).

%% @edoc Find by module
-spec find_by_module(atom()) -> {ok, [item()]} | {error, any()}.
find_by_module(Module) ->
  Pattern = #els_dt_module{module = Module, _ = '_'},
  find_by(Pattern).

%% TODO: Improve efficiency
-spec find_by(tuple()) -> {ok, [item()]} | {error, any()}.
find_by(Pattern) ->
  case els_db:match(Pattern) of
    {ok, Items} ->
      {ok, [to_map(Item) || Item <- Items]};
    {error, Error} ->
      {error, Error}
  end.
