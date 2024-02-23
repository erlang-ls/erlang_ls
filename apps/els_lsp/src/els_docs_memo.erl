%%==============================================================================
%% The 'docs' table
%%==============================================================================

-module(els_docs_memo).

%%==============================================================================
%% Behaviour els_db_table
%%==============================================================================

-behaviour(els_db_table).
-export([
    name/0,
    opts/0
]).

%%==============================================================================
%% API
%%==============================================================================

-export([
    insert/1,
    lookup/1,
    delete_by_uri/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_docs_memo, {
    mfact :: mfact() | '_' | {atom(), '_', '_', call_type() | '_', function | type | '_'},
    entries :: [els_markup_content:doc_entry()] | '_'
}).
-type call_type() :: 'local' | 'remote'.
-type mfact() :: {module(), atom(), arity(), call_type(), function | type}.
-type els_docs_memo() :: #els_docs_memo{}.
-type item() :: #{
    mfact := mfact(),
    entries := [els_markup_content:doc_entry()]
}.
-export_type([item/0]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
    [ordered_set].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_docs_memo().
from_item(#{
    mfact := MFACT,
    entries := Entries
}) ->
    #els_docs_memo{
        mfact = MFACT,
        entries = Entries
    }.

-spec to_item(els_docs_memo()) -> item().
to_item(#els_docs_memo{
    mfact = MFACT,
    entries = Entries
}) ->
    #{
        mfact => MFACT,
        entries => Entries
    }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
    Record = from_item(Map),
    els_db:write(name(), Record).

-spec lookup(mfact()) -> {ok, [item()]}.
lookup({M, _F, _A, _C, _T} = MFACT) ->
    {ok, _Uris} = els_utils:find_modules(M),
    {ok, Items} = els_db:lookup(name(), MFACT),
    {ok, [to_item(Item) || Item <- Items]}.

-spec delete_by_uri(uri()) -> ok.
delete_by_uri(Uri) ->
    case filename:extension(Uri) of
        <<".erl">> ->
            Module = els_uri:module(Uri),
            Pattern = #els_docs_memo{mfact = {Module, '_', '_', '_', '_'}, _ = '_'},
            ok = els_db:match_delete(name(), Pattern);
        _ ->
            ok
    end.
