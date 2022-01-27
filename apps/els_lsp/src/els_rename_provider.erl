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
workspace_edits(Uri, [#{kind := function_clause} = POI| _], NewName) ->
  #{id := {F, A, _}} = POI,
  #{changes => changes(Uri, POI#{kind => function, id => {F, A}}, NewName)};
workspace_edits(Uri, [#{kind := Kind} = POI| _], NewName)
  when Kind =:= define;
       Kind =:= record;
       Kind =:= record_def_field;
       Kind =:= function;
       Kind =:= type_definition;
       Kind =:= variable ->
  #{changes => changes(Uri, POI, NewName)};
workspace_edits(Uri, [#{kind := Kind} = POI| _], NewName)
  when Kind =:= macro;
       Kind =:= record_expr;
       Kind =:= record_field;
       Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= export_entry;
       Kind =:= import_entry;
       Kind =:= export_type_entry;
       Kind =:= type_application ->
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
editable_range(#{kind := Kind, data := #{name_range := Range}})
  when Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= callback;
       Kind =:= spec;
       Kind =:= export_entry;
       Kind =:= export_type_entry;
       Kind =:= import_entry;
       Kind =:= type_application;
       Kind =:= type_definition ->
  %% application POI of a local call and
  %% type_application POI of a built-in type don't have name_range data
  %% they are handled by the next clause
  els_protocol:range(Range);
editable_range(#{kind := _Kind, range := Range}) ->
  els_protocol:range(Range).

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
changes(Uri, #{kind := type_definition, id := {Name, A}}, NewName) ->
  ?LOG_INFO("Renaming type ~p/~p to ~s", [Name, A, NewName]),
  {ok, Doc} = els_utils:lookup_document(Uri),
  SelfChanges = [change(P, NewName) ||
                  P <- els_dt_document:pois(Doc, [ type_definition
                                                 , export_type_entry
                                                 ]),
                  maps:get(id, P) =:= {Name, A}
                ],
  Key = {els_uri:module(Uri), Name, A},
  {ok, Refs} = els_dt_references:find_by_id(type_application, Key),
  RefPOIs = convert_references_to_pois(Refs, [ type_application
                                             ]),
  Changes =
    lists:foldl(
    fun({RefUri, RefPOI}, Acc) ->
        Change = change(RefPOI, NewName),
        maps:update_with(RefUri, fun(V) -> [Change|V] end, [Change], Acc)
    end, #{Uri => SelfChanges}, RefPOIs),
  ?LOG_INFO("Done renaming type ~p/~p to ~s. ~p changes in ~p files.",
            [Name, A, NewName, length(lists:flatten(maps:values(Changes))),
             length(maps:keys(Changes))]),
  Changes;
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
changes(Uri, #{kind := DefKind} = DefPoi, NewName)
  when DefKind =:= define;
       DefKind =:= record;
       DefKind =:= record_def_field ->
  Self = #{range => editable_range(DefPoi), newText => NewName},
  Refs = els_references_provider:find_scoped_references_for_def(Uri, DefPoi),
  lists:foldl(
    fun({U, Poi}, Acc) ->
        Change = #{ range => editable_range(Poi)
                  , newText =>  new_name(Poi, NewName)
                  },
        maps:update_with(U, fun(V) -> [Change|V] end, [Change], Acc)
    end, #{Uri => [Self]}, Refs);
changes(_Uri, _POI, _NewName) ->
  null.

-spec new_name(poi(), binary()) -> binary().
new_name(#{kind := macro}, NewName) ->
  <<"?", NewName/binary>>;
new_name(#{kind := record_expr}, NewName) ->
  <<"#", NewName/binary>>;
new_name(_, NewName) ->
  NewName.

-spec function_clause_range(poi_range(), els_dt_document:item()) -> poi_range().
function_clause_range(VarRange, Document) ->
  Attributes = [spec, callback, define, record, type_definition],
  AttrPOIs = els_dt_document:pois(Document, Attributes),
  POIs = els_poi:sort(els_dt_document:pois(Document, [ function_clause
                                                     | Attributes
                                                     ])),
  case [R || #{range := R} <- AttrPOIs, els_range:in(VarRange, R)] of
    [AttrRange] ->
      %% Renaming variable in spec
      AttrRange;
    [] ->
      %% Find beginning of first function clause before VarRange
      From =
        case [R || #{range := R} <- POIs, els_range:compare(R, VarRange)] of
          []        -> {0, 0}; % Beginning of document
          FunRanges -> maps:get(from, lists:last(FunRanges))
        end,
      %% Find beginning of first function clause after VarRange
      To =
        case [R || #{range := R} <- POIs, els_range:compare(VarRange, R)] of
          []                 -> {999999999, 999999999}; % End of document
          [#{from := End}|_] -> End
        end,
      #{from => From, to => To}
  end.

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
  #{range => editable_range(POI), newText => NewName}.
