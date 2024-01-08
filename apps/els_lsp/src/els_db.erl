-module(els_db).

%% API
-export([
    clear_table/1,
    clear_tables/0,
    delete/2,
    delete_object/2,
    lookup/2,
    match/2,
    match_delete/2,
    select_delete/2,
    tables/0,
    write/2,
    conditional_write/4
]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type condition() :: fun((tuple()) -> boolean()).
-export_type([condition/0]).

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec tables() -> [atom()].
tables() ->
    [
        els_dt_document,
        els_dt_document_index,
        els_dt_references,
        els_dt_signatures,
        els_docs_memo
    ].

-spec delete(atom(), any()) -> ok.
delete(Table, Key) ->
    els_db_server:delete(Table, Key).

-spec delete_object(atom(), any()) -> ok.
delete_object(Table, Object) ->
    els_db_server:delete_object(Table, Object).

-spec lookup(atom(), any()) -> {ok, [tuple()]}.
lookup(Table, Key) ->
    {ok, ets:lookup(Table, Key)}.

-spec match(atom(), tuple()) -> {ok, [tuple()]}.
match(Table, Pattern) when is_tuple(Pattern) ->
    {ok, ets:match_object(Table, Pattern)}.

-spec match_delete(atom(), tuple()) -> ok.
match_delete(Table, Pattern) when is_tuple(Pattern) ->
    els_db_server:match_delete(Table, Pattern).

-spec select_delete(atom(), any()) -> ok.
select_delete(Table, MS) ->
    els_db_server:select_delete(Table, MS).

-spec write(atom(), tuple()) -> ok.
write(Table, Object) when is_tuple(Object) ->
    els_db_server:write(Table, Object).

-spec conditional_write(atom(), any(), tuple(), condition()) ->
    ok | {error, any()}.
conditional_write(Table, Key, Object, Condition) when is_tuple(Object) ->
    els_db_server:conditional_write(Table, Key, Object, Condition).

-spec clear_table(atom()) -> ok.
clear_table(Table) ->
    els_db_server:clear_table(Table).

-spec clear_tables() -> ok.
clear_tables() ->
    [ok = clear_table(T) || T <- tables()],
    ok.
