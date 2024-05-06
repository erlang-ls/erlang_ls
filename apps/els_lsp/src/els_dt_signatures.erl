%%==============================================================================
%% The 'signatures' table
%%==============================================================================

-module(els_dt_signatures).

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
    versioned_insert/1,
    lookup/1,
    delete_by_uri/1,
    versioned_delete_by_uri/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%==============================================================================
%% Item Definition
%%==============================================================================

-record(els_dt_signatures, {
    mfa :: mfa() | '_' | {atom(), '_', '_'},
    spec :: binary() | '_',
    version :: version() | '_',
    args :: els_arg:args() | '_'
}).
-type els_dt_signatures() :: #els_dt_signatures{}.
-type version() :: null | integer().
-type item() :: #{
    mfa := mfa(),
    spec := binary(),
    version := version(),
    args := els_arg:args()
}.
-export_type([item/0]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
    [set].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_dt_signatures().
from_item(#{
    mfa := MFA,
    spec := Spec,
    version := Version,
    args := Args
}) ->
    #els_dt_signatures{
        mfa = MFA,
        spec = Spec,
        version = Version,
        args = Args
    }.

-spec to_item(els_dt_signatures()) -> item().
to_item(#els_dt_signatures{
    mfa = MFA,
    spec = Spec,
    version = Version,
    args = Args
}) ->
    #{
        mfa => MFA,
        spec => Spec,
        version => Version,
        args => Args
    }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
    Record = from_item(Map),
    els_db:write(name(), Record).

-spec versioned_insert(item()) -> ok | {error, any()}.
versioned_insert(#{mfa := MFA, version := Version} = Map) ->
    Record = from_item(Map),
    Condition = fun(#els_dt_signatures{version = CurrentVersion}) ->
        CurrentVersion =:= null orelse Version >= CurrentVersion
    end,
    els_db:conditional_write(name(), MFA, Record, Condition).

-spec lookup(mfa()) -> {ok, [item()]}.
lookup({M, _F, _A} = MFA) ->
    {ok, _Uris} = els_utils:find_modules(M),
    {ok, Items} = els_db:lookup(name(), MFA),
    {ok, [to_item(Item) || Item <- Items]}.

-spec delete_by_uri(uri()) -> ok.
delete_by_uri(Uri) ->
    case filename:extension(Uri) of
        <<".erl">> ->
            Module = els_uri:module(Uri),
            Pattern = #els_dt_signatures{mfa = {Module, '_', '_'}, _ = '_'},
            ok = els_db:match_delete(name(), Pattern);
        _ ->
            ok
    end.

-spec versioned_delete_by_uri(uri(), version()) -> ok.
versioned_delete_by_uri(Uri, Version) ->
    case filename:extension(Uri) of
        <<".erl">> ->
            Module = els_uri:module(Uri),
            MS = ets:fun2ms(
                fun
                    (#els_dt_signatures{mfa = {M, _, _}, version = CurrentVersion}) when
                        M =:= Module,
                        CurrentVersion =:= null orelse CurrentVersion < Version
                    ->
                        true;
                    (_) ->
                        false
                end
            ),
            ok = els_db:select_delete(name(), MS);
        _ ->
            ok
    end.
