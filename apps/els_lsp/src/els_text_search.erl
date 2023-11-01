%%==============================================================================
%% Text-based search
%%==============================================================================
-module(els_text_search).

%%==============================================================================
%% API
%%==============================================================================
-export([find_candidate_uris/1]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec find_candidate_uris({els_dt_references:poi_category(), any()}) -> [uri()].
find_candidate_uris(Id) ->
    Pattern = extract_pattern(Id),
    els_dt_document:find_candidates(Pattern).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec extract_pattern({els_dt_references:poi_category(), any()}) ->
    atom() | binary().
extract_pattern({function, {_M, F, _A}}) ->
    F;
extract_pattern({type, {_M, F, _A}}) ->
    F;
extract_pattern({macro, {Name, _Arity}}) ->
    Name;
extract_pattern({macro, Name}) ->
    Name;
extract_pattern({record, Record}) ->
    Record;
extract_pattern({record_field, {Record, _Field}}) ->
    Record;
extract_pattern({include, Id}) ->
    include_id(Id);
extract_pattern({include_lib, Id}) ->
    include_id(Id);
extract_pattern({behaviour, Name}) ->
    Name.

-spec include_id(string()) -> string().
include_id(Id) ->
    filename:rootname(filename:basename(Id)).
