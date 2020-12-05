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
-spec workspace_edits(uri(), [poi()], binary()) -> [any()]. %% TODO: Refine type
workspace_edits(_Uri, [], _NewName) ->
  null;
workspace_edits(Uri, [#{kind := callback} = POI | _], NewName) ->
  #{id := {_FunctionName, Arity} = Id, range := Range} = POI,
  Module = els_uri:module(Uri),
  {ok, Refs} = els_dt_references:find_by_id(behaviour, Module),
  Changes =
    lists:foldl(
      fun(#{uri := U}, Acc) ->
          %% TODO: Modify actual functions (and all clauses)
          {ok, Doc} = els_utils:lookup_document(U),
          ExportEntries = els_dt_document:pois(Doc, [export_entry]),
          Acc#{ U =>
                  [ #{ range => editable_range(export_entry, R)
                     , newText =>
                         unicode:characters_to_binary([ NewName
                                                      , "/"
                                                      , integer_to_list(Arity)
                                                      ])
                     } || #{ id := I
                           , range := R
                           } <- ExportEntries
                            , I =:= Id
                  ]
              }
      end, #{ Uri =>
                [#{ range => editable_range(callback, Range)
                  , newText => NewName
                  }]
            }, Refs),
  #{changes => Changes};
workspace_edits(_Uri, _POIs, _NewName) ->
  null.

-spec editable_range(poi_kind(), poi_range()) -> range().
editable_range(callback, Range) ->
  #{ from := {FromL, FromC}, to := {ToL, ToC} } = Range,
  #{ start => #{line => FromL - 1, character => FromC + length("callback")}
   , 'end' => #{line => ToL - 1,   character => ToC}
   };
editable_range(_, Range) ->
  #{ from := {FromL, FromC}, to := {ToL, ToC} } = Range,
  #{ start => #{line => FromL - 1, character => FromC}
   , 'end' => #{line => ToL - 1,   character => ToC}
   }.
