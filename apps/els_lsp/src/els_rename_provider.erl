-module(els_rename_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Defines
%%==============================================================================

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({rename, Params}, State) ->
  #{ <<"textDocument">> := #{<<"uri">> := Uri}
   , <<"position">> := #{ <<"line">>      := Line
                        , <<"character">> := Character
                        }
   , <<"newName">> := NewName
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  Elem = els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1),
  WorkspaceEdits = workspace_edits(Uri, Elem, NewName),
  {WorkspaceEdits, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec workspace_edits(uri(), [poi()], binary()) -> null | [any()].
workspace_edits(_Uri, [], _NewName) ->
  null;
workspace_edits(Uri, [#{kind := variable} = POI| _], NewName) ->
  #{changes => changes(Uri, POI, NewName)};
workspace_edits(Uri, [#{kind := function} = POI| _], NewName) ->
  #{changes => changes(Uri, POI, NewName)};
workspace_edits(Uri, [#{kind := 'define'} = POI| _], NewName) ->
  #{changes => changes(Uri, POI, NewName)};
workspace_edits(Uri, [#{kind := 'macro'} = POI| _], NewName) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, DefUri, DefPOI} ->
      #{changes => changes(DefUri, DefPOI, NewName)};
    _ ->
      null
  end;
workspace_edits(Uri, [#{kind := 'callback'} = POI | _], NewName) ->
  #{id := {Name, Arity} = Id} = POI,
  Module = els_uri:module(Uri),
  {ok, Refs} = els_dt_references:find_by_id(behaviour, Module),
  Changes =
    lists:foldl(
      fun(#{uri := U}, Acc) ->
          {ok, Doc} = els_utils:lookup_document(U),
          ExportEntries = els_dt_document:pois(Doc, [export_entry]),
          FunctionClauses = els_dt_document:pois(Doc, [function_clause]),
          Specs = els_dt_document:pois(Doc, [spec]),
          Acc#{ U =>
                  [ #{ range => editable_range(P)
                     , newText => NewName
                     } || #{id := I} = P <- ExportEntries, I =:= Id
                  ] ++
                  [ #{ range => editable_range(P)
                     , newText => NewName
                     } || #{id := {N, A, _I}} = P <- FunctionClauses
                            , N =:= Name
                            , A =:= Arity
                  ] ++
                  [ #{ range => editable_range(P)
                     , newText => NewName
                     } || #{id := I} = P <- Specs, I =:= Id
                  ]
              }
      end, #{ Uri =>
                [#{ range => editable_range(POI)
                  , newText => NewName
                  }]
            }, Refs),
  #{changes => Changes};
workspace_edits(_Uri, _POIs, _NewName) ->
  null.

-spec editable_range(poi()) -> range().
editable_range(#{kind := callback, range := Range}) ->
  #{ from := {FromL, FromC} } = Range,
  EditFromC = FromC + string:length("-callback "),
  els_protocol:range(Range#{ from := {FromL, EditFromC } });
editable_range(#{kind := export_entry, id := {F, _A}, range := Range}) ->
  #{ from := {FromL, FromC} } = Range,
  EditToC = FromC + string:length(atom_to_string(F)),
  els_protocol:range(Range#{ to := {FromL, EditToC} });
editable_range(#{kind := spec, id := {F, _A}, range := Range}) ->
  #{ from := {FromL, FromC}, to := {_ToL, _ToC} } = Range,
  EditFromC = FromC + string:length("-spec "),
  EditToC = EditFromC + string:length(atom_to_string(F)),
  els_protocol:range(Range#{ from := {FromL, EditFromC}
                           , to := {FromL, EditToC} });
editable_range(#{kind := implicit_fun, id := {M, F, _A}, range := Range}) ->
  #{ from := {FromL, FromC}, to := {_ToL, _ToC} } = Range,
  EditFromC = FromC + length("fun " ++ atom_to_string(M) ++ ":"),
  EditToC = EditFromC + length(atom_to_string(F)),
  els_protocol:range(#{ from => {FromL, EditFromC}
                      , to => {FromL, EditToC} });
editable_range(#{kind := implicit_fun, id := {F, _A}, range := Range}) ->
  #{ from := {FromL, FromC}, to := {_ToL, _ToC} } = Range,
  EditFromC = FromC + length("fun "),
  EditToC = EditFromC + length(atom_to_string(F)),
  els_protocol:range(#{ from => {FromL, EditFromC}
                      , to => {FromL, EditToC} });
editable_range(#{kind := import_entry, id := {_M, F, _A}, range := Range}) ->
  #{ from := {FromL, FromC} } = Range,
  EditToC = FromC + length(atom_to_string(F)),
  els_protocol:range(Range#{ to := {FromL, EditToC} });
editable_range(#{kind := application, id := {M, F, _A}, range := Range}) ->
  #{ from := {FromL, FromC}, to := {_ToL, _ToC} } = Range,
  EditFromC = FromC + length(atom_to_string(M) ++ ":"),
  EditToC = EditFromC + length(atom_to_string(F)),
  els_protocol:range(#{ from => {FromL, EditFromC}
                      , to => {FromL, EditToC} });
editable_range(#{kind := _Kind, range := Range}) ->
  els_protocol:range(Range).

-spec editable_range(poi_kind(), els_dt_references:item()) -> range().
editable_range(macro, #{range := Range}) ->
  #{ from := {FromL, FromC} } = Range,
  EditFromC = FromC + string:length("?"),
  els_protocol:range(Range#{ from := {FromL, EditFromC} }).

-spec changes(uri(), poi(), binary()) -> #{uri() => [text_edit()]} | null.
changes(Uri, #{kind := variable, id := VarId, range := VarRange}, NewName) ->
  %% Rename variable in function clause scope
  case els_utils:lookup_document(Uri) of
    {ok, Document} ->
      FunRange = function_clause_range(VarRange, Document),
      Changes = [#{range => editable_range(POI), newText => NewName} ||
                  POI <- els_dt_document:pois(Document, [variable]),
                  maps:get(id, POI) =:= VarId,
                  els_range:in(maps:get(range, POI), FunRange)
                ],
      #{Uri => Changes};
    {error, _} ->
      null
  end;
changes(Uri, #{kind := function, id := {F, A}}, NewName) ->
  ?LOG_INFO("Renaming function ~p/~p to ~s", [F, A, NewName]),
  {ok, Doc} = els_utils:lookup_document(Uri),
  IsMatch = fun (#{id := {Fun, Arity}}) -> {Fun, Arity} =:= {F, A};
                (#{id := {Fun, Arity, _}}) -> {Fun, Arity} =:= {F, A};
                (_) -> false
            end,
  SelfChanges = [change(P, NewName) ||
                  P <- els_dt_document:pois(Doc, [ export_entry
                                                 , function_clause
                                                 , spec
                                                 ]),
                  IsMatch(P)
                ],
  Key = {els_uri:module(Uri), F, A},
  {ok, Refs} = els_dt_references:find_by_id(function, Key),
  RefPOIs = convert_references_to_pois(Refs, [ application
                                             , implicit_fun
                                             , import_entry
                                             ]),
  Changes =
    lists:foldl(
    fun({RefUri, RefPOI}, Acc) ->
        ImportChanges = import_changes(RefUri, RefPOI, NewName),
        Changes = [change(RefPOI, NewName)] ++ ImportChanges,
        maps:update_with(RefUri, fun(V) -> Changes ++ V end, Changes, Acc)
    end, #{Uri => SelfChanges}, RefPOIs),
  ?LOG_INFO("Done renaming function ~p/~p to ~s. ~p changes in ~p files.",
            [F, A, NewName, length(lists:flatten(maps:values(Changes))),
             length(maps:keys(Changes))]),
  Changes;
changes(Uri, #{kind := 'define', id := Id} = POI, NewName) ->
  Self = #{range => editable_range(POI), newText => NewName},
  {ok, Refs} = els_dt_references:find_by_id(macro, Id),
  lists:foldl(
    fun(#{uri := U} = Ref, Acc) ->
        Change = #{ range => editable_range(macro, Ref)
                  , newText => NewName
                  },
        maps:update_with(U, fun(V) -> [Change|V] end, [Change], Acc)
    end, #{Uri => [Self]}, Refs);
changes(_Uri, _POI, _NewName) ->
  null.


-spec function_clause_range(poi_range(), els_dt_document:item()) -> poi_range().
function_clause_range(VarRange, Document) ->
  FunPOIs = els_poi:sort(els_dt_document:pois(Document, [function_clause])),
  %% Find beginning of first function clause before VarRange
  From = case [R || #{range := R} <- FunPOIs, els_range:compare(R, VarRange)] of
           []        -> {0, 0}; % Beginning of document
           FunRanges -> maps:get(from, lists:last(FunRanges))
         end,
  %% Find beginning of first function clause after VarRange
  To = case [R || #{range := R} <- FunPOIs, els_range:compare(VarRange, R)] of
        []                 -> {999999999, 999999999}; % End of document
        [#{from := End}|_] -> End
       end,
  #{from => From, to => To}.

-spec convert_references_to_pois([els_dt_references:item()], [poi_kind()]) ->
        [{uri(), poi()}].
convert_references_to_pois(Refs, Kinds) ->
  UriPOIs = lists:foldl(fun(#{uri := Uri}, Acc) when is_map_key(Uri, Acc) ->
                            Acc;
                           (#{uri := Uri}, Acc) ->
                            POIs = case els_utils:lookup_document(Uri) of
                                     {ok, Doc} ->
                                       els_dt_document:pois(Doc, Kinds);
                                     {error, _} ->
                                       []
                                   end,
                            maps:put(Uri, POIs, Acc)
                        end, #{}, Refs),
  lists:map(fun(#{uri := Uri, range := #{from := Pos}}) ->
                POIs = maps:get(Uri, UriPOIs),
                {Uri, hd(els_poi:match_pos(POIs, Pos))}
            end, Refs).

%% @doc Find all uses of imported function in Uri
-spec import_changes(uri(), poi(), binary()) -> [text_edit()].
import_changes(Uri, #{kind := import_entry, id := {_M, F, A}}, NewName) ->
  case els_utils:lookup_document(Uri) of
    {ok, Doc} ->
      [change(P, NewName) || P <- els_dt_document:pois(Doc, [ application
                                                            , implicit_fun
                                                            ]),
                             maps:get(id, P) =:= {F, A}];
    {error, _} ->
      []
  end;
import_changes(_Uri, _POI, _NewName) ->
  [].

-spec change(poi(), binary()) -> text_edit().
change(POI, NewName) ->
  %% TODO: editable_range expects only `fun foo/0' in source so
  %% so for example if `fun   foo/0' is in the source it will result in invalid
  %% changes as the text that the POI corresponds to is lost.
  %% This is a deep fundamental flaw in how POIs are parsed and need to be
  %% addressed in the parser
  #{range => editable_range(POI), newText => NewName}.

-spec atom_to_string(atom()) -> string().
atom_to_string(Atom) ->
  io_lib:write(Atom).
