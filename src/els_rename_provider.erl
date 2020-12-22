-module(els_rename_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

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
  EditFromC = FromC + length("-callback "),
  els_protocol:range(Range#{ from := {FromL, EditFromC } });
editable_range(#{kind := export_entry, id := {F, _A}, range := Range}) ->
  #{ from := {FromL, FromC} } = Range,
  EditToC = FromC + length(atom_to_list(F)),
  els_protocol:range(Range#{ to := {FromL, EditToC} });
editable_range(#{kind := spec, id := {F, _A}, range := Range}) ->
  #{ from := {FromL, FromC}, to := {_ToL, _ToC} } = Range,
  EditFromC = FromC + length("-spec "),
  EditToC = EditFromC + length(atom_to_list(F)),
  els_protocol:range(Range#{ from := {FromL, EditFromC}
                           , to := {FromL, EditToC} });
editable_range(#{kind := _Kind, range := Range}) ->
  els_protocol:range(Range).

-spec editable_range(poi_kind(), els_dt_references:item()) -> range().
editable_range(macro, #{range := Range}) ->
  #{ from := {FromL, FromC}, to := {ToL, ToC} } = Range,
  #{ start => #{line => FromL - 1, character => FromC}
   , 'end' => #{line => ToL - 1,   character => ToC}
   }.

-spec changes(uri(), poi(), binary()) -> #{uri() => [text_edit()]} | null.
changes(Uri, #{kind := 'define', id := Id} = POI, NewName) ->
  Self = #{range => editable_range(POI), newText => NewName},
  {ok, Refs} = els_dt_references:find_by_id(macro, Id),
  lists:foldl(
    fun(#{uri := U} = Ref, Acc) ->
        Change = #{ range => editable_range(macro, Ref)
                  , newText => NewName
                  },
        case maps:is_key(U, Acc) of
          false ->
            maps:put(U, [Change], Acc);
          true ->
            maps:put(U, [Change|maps:get(U, Acc)], Acc)
        end
    end, #{Uri => [Self]}, Refs);
changes(_Uri, _POI, _NewName) ->
  null.
