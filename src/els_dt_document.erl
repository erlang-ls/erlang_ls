%%==============================================================================
%% The 'document' table
%%==============================================================================

-module(els_dt_document).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-export([ name/0
        , opts/0
        ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ insert/1
        , lookup/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_document, { uri      :: uri()  | '_'
                         , document :: els_document:document()
                         }).
-type els_dt_document() :: #els_dt_document{}.

-type item() :: #{ uri      := uri()
                 , document := els_document:document()
                 }.
-export_type([ item/0 ]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
  [ {attributes , record_info(fields, els_dt_document)}
  , {disc_copies, [node()]}
  , {index      , []}
  , {type       , set}
  ].

%%==============================================================================
%% API
%%==============================================================================

-spec from_map(item()) -> els_dt_document().
from_map(#{ uri := Uri, document := Document }) ->
  #els_dt_document{ uri = Uri, document = Document }.

-spec to_map(els_dt_document()) -> item().
to_map(#els_dt_document{ uri = Uri, document = Document }) ->
  #{ uri      => Uri
   , document => Document
   }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
  Record = from_map(Map),
  els_db:write(Record).

-spec lookup(uri()) -> {ok, [item()]} | {error, any()}.
lookup(Uri) ->
  case els_db:lookup(name(), Uri) of
    {ok, Items} ->
      {ok, [to_map(Item) || Item <- Items]};
    {error, Reason} ->
      {error, Reason}
  end.
