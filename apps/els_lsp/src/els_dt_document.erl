%%==============================================================================
%% The 'document' table
%%==============================================================================

-module(els_dt_document).

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
    delete/1
]).

-export([
    new/3,
    new/4,
    pois/1,
    pois/2,
    get_element_at_pos/3,
    uri/1,
    functions_at_pos/3,
    applications_at_pos/3,
    wrapping_functions/2,
    wrapping_functions/3,
    find_candidates/1,
    get_words/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type id() :: atom().
-type kind() :: module | header | other.
-type source() :: otp | app | dep.
-type version() :: null | integer().
-export_type([source/0]).

%%==============================================================================
%% Item Definition
%%==============================================================================
-record(els_dt_document, {
    uri :: uri() | '_' | '$1',
    id :: id() | '_',
    kind :: kind() | '_',
    text :: binary() | '_',
    pois :: [els_poi:poi()] | '_' | ondemand,
    source :: source() | '$2',
    words :: sets:set() | '_' | '$3',
    version :: version() | '_'
}).
-type els_dt_document() :: #els_dt_document{}.

-type item() :: #{
    uri := uri(),
    id := id(),
    kind := kind(),
    text := binary(),
    pois => [els_poi:poi()] | ondemand,
    source => source(),
    words => sets:set(),
    version => version()
}.
-export_type([
    id/0,
    item/0,
    kind/0
]).

%%==============================================================================
%% Callbacks for the els_db_table Behaviour
%%==============================================================================

-spec name() -> atom().
name() -> ?MODULE.

-spec opts() -> proplists:proplist().
opts() ->
    [set, compressed].

%%==============================================================================
%% API
%%==============================================================================

-spec from_item(item()) -> els_dt_document().
from_item(#{
    uri := Uri,
    id := Id,
    kind := Kind,
    text := Text,
    pois := POIs,
    source := Source,
    words := Words,
    version := Version
}) ->
    #els_dt_document{
        uri = Uri,
        id = Id,
        kind = Kind,
        text = Text,
        pois = POIs,
        source = Source,
        words = Words,
        version = Version
    }.

-spec to_item(els_dt_document()) -> item().
to_item(#els_dt_document{
    uri = Uri,
    id = Id,
    kind = Kind,
    text = Text,
    pois = POIs,
    source = Source,
    words = Words,
    version = Version
}) ->
    #{
        uri => Uri,
        id => Id,
        kind => Kind,
        text => Text,
        pois => POIs,
        source => Source,
        words => Words,
        version => Version
    }.

-spec insert(item()) -> ok | {error, any()}.
insert(Map) when is_map(Map) ->
    Record = from_item(Map),
    els_db:write(name(), Record).

-spec versioned_insert(item()) -> ok | {error, any()}.
versioned_insert(#{uri := Uri, version := Version} = Map) ->
    Record = from_item(Map),
    Condition = fun(#els_dt_document{version = CurrentVersion}) ->
        CurrentVersion =:= null orelse Version >= CurrentVersion
    end,
    els_db:conditional_write(name(), Uri, Record, Condition).

-spec lookup(uri()) -> {ok, [item()]}.
lookup(Uri) ->
    {ok, Items} = els_db:lookup(name(), Uri),
    {ok, [to_item(Item) || Item <- Items]}.

-spec delete(uri()) -> ok.
delete(Uri) ->
    els_db:delete(name(), Uri).

-spec new(uri(), binary(), source()) -> item().
new(Uri, Text, Source) ->
    new(Uri, Text, Source, _Version = null).

-spec new(uri(), binary(), source(), version()) -> item().
new(Uri, Text, Source, Version) ->
    Extension = filename:extension(Uri),
    Id = binary_to_atom(filename:basename(Uri, Extension), utf8),
    case Extension of
        <<".erl">> ->
            new(Uri, Text, Id, module, Source, Version);
        <<".hrl">> ->
            new(Uri, Text, Id, header, Source, Version);
        _ ->
            new(Uri, Text, Id, other, Source, Version)
    end.

-spec new(uri(), binary(), atom(), kind(), source(), version()) -> item().
new(Uri, Text, Id, Kind, Source, Version) ->
    #{
        uri => Uri,
        id => Id,
        kind => Kind,
        text => Text,
        pois => ondemand,
        source => Source,
        words => get_words(Text),
        version => Version
    }.

%% @doc Returns the list of POIs for the current document
-spec pois(item()) -> [els_poi:poi()].
pois(#{uri := Uri, pois := ondemand}) ->
    #{pois := POIs} = els_indexing:ensure_deeply_indexed(Uri),
    POIs;
pois(#{pois := POIs}) ->
    POIs.

%% @doc Returns the list of POIs of the given types for the current
%%      document
-spec pois(item(), [els_poi:poi_kind()]) -> [els_poi:poi()].
pois(Item, Kinds) ->
    [POI || #{kind := K} = POI <- pois(Item), lists:member(K, Kinds)].

-spec get_element_at_pos(item(), non_neg_integer(), non_neg_integer()) ->
    [els_poi:poi()].
get_element_at_pos(Item, Line, Column) ->
    POIs = pois(Item),
    MatchedPOIs = els_poi:match_pos(POIs, {Line, Column}),
    els_poi:sort(MatchedPOIs).

%% @doc Returns the URI of the current document
-spec uri(item()) -> uri().
uri(#{uri := Uri}) ->
    Uri.

-spec functions_at_pos(item(), non_neg_integer(), non_neg_integer()) -> [els_poi:poi()].
functions_at_pos(Item, Line, Column) ->
    POIs = get_element_at_pos(Item, Line, Column),
    [POI || #{kind := 'function'} = POI <- POIs].

-spec applications_at_pos(item(), non_neg_integer(), non_neg_integer()) ->
    [els_poi:poi()].
applications_at_pos(Item, Line, Column) ->
    POIs = get_element_at_pos(Item, Line, Column),
    [POI || #{kind := 'application'} = POI <- POIs].

-spec wrapping_functions(item(), non_neg_integer(), non_neg_integer()) ->
    [els_poi:poi()].
wrapping_functions(Document, Line, Column) ->
    Range = #{from => {Line, Column}, to => {Line, Column}},
    Functions = pois(Document, ['function']),
    [
        F
     || #{data := #{wrapping_range := WR}} = F <- Functions,
        els_range:in(Range, WR)
    ].

-spec wrapping_functions(item(), range()) -> [els_poi:poi()].
wrapping_functions(Document, Range) ->
    #{start := #{character := Character, line := Line}} = Range,
    wrapping_functions(Document, Line, Character).

-spec find_candidates(atom() | string()) -> [uri()].
find_candidates(Pattern) ->
    %% ets:fun2ms(fun(#els_dt_document{source = Source, uri = Uri, words = Words})
    %% when Source =/= otp -> {Uri, Words} end).
    MS = [
        {
            #els_dt_document{
                uri = '$1',
                id = '_',
                kind = '_',
                text = '_',
                pois = '_',
                source = '$2',
                words = '$3',
                version = '_'
            },
            [{'=/=', '$2', otp}],
            [{{'$1', '$3'}}]
        }
    ],
    All = ets:select(name(), MS),
    Fun = fun({Uri, Words}) ->
        case sets:is_element(Pattern, Words) of
            true -> {true, Uri};
            false -> false
        end
    end,
    lists:filtermap(Fun, All).

-spec get_words(binary()) -> sets:set().
get_words(Text) ->
    case erl_scan:string(els_utils:to_list(Text)) of
        {ok, Tokens, _EndLocation} ->
            Fun = fun
                ({atom, _Location, Atom}, Words) ->
                    sets:add_element(Atom, Words);
                ({string, _Location, String}, Words) ->
                    case filename:extension(String) of
                        ".hrl" ->
                            Id = filename:rootname(filename:basename(String)),
                            sets:add_element(Id, Words);
                        _ ->
                            Words
                    end;
                (_, Words) ->
                    Words
            end,
            lists:foldl(Fun, sets:new(), Tokens);
        {error, ErrorInfo, _ErrorLocation} ->
            ?LOG_DEBUG("Errors while get_words ~p", [ErrorInfo])
    end.
