%%==============================================================================
%% Text-based search
%%==============================================================================
-module(els_text_search).

%%==============================================================================
%% API
%%==============================================================================
-export([ find_candidate_uris/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec find_candidate_uris({els_dt_references:poi_category(), any()}) -> [uri()].
find_candidate_uris(Id) ->
  String = id_to_binary(Id),
  els_dt_document:find_candidates(String).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec id_to_binary({els_dt_references:poi_category(), any()}) -> binary().
id_to_binary({function, {_M, F, _A}}) ->
  atom_to_binary(F, utf8);
id_to_binary({type, {_M, F, _A}}) ->
  atom_to_binary(F, utf8);
id_to_binary({macro, {Name, _Arity}}) ->
  atom_to_binary(Name, utf8);
id_to_binary({macro, Name}) ->
  atom_to_binary(Name, utf8);
id_to_binary({include, Id}) ->
  include_id_to_binary(Id);
id_to_binary({include_lib, Id}) ->
  include_id_to_binary(Id);
id_to_binary({behaviour, Name}) ->
  atom_to_binary(Name, utf8).

-spec include_id_to_binary(string()) -> binary().
include_id_to_binary(Id) ->
  els_utils:to_binary(filename:rootname(filename:basename(Id))).
