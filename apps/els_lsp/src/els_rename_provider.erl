-module(els_rename_provider).

-behaviour(els_provider).

-export([
    handle_request/1,
    options/0
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

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {response, any()}.
handle_request({rename, Params}) ->
    #{
        <<"textDocument">> := #{<<"uri">> := Uri},
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"newName">> := NewName
    } = Params,
    {ok, Document} = els_utils:lookup_document(Uri),
    Elem = els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1),
    WorkspaceEdits = workspace_edits(Uri, Elem, NewName),
    {response, WorkspaceEdits}.

-spec options() -> boolean() | map().
options() ->
    case els_config:get(capabilities) of
        #{<<"textDocument">> := #{<<"rename">> := #{<<"prepareSupport">> := true}}} ->
            #{prepareProvider => true};
        _ ->
            true
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec workspace_edits(
    uri(),
    [els_poi:poi()],
    binary()
) -> null | [any()].
workspace_edits(_Uri, [], _NewName) ->
    null;
workspace_edits(OldUri, [#{kind := module} = POI | _], NewName) ->
    %% Generate new Uri
    Path = els_uri:path(OldUri),
    Dir = filename:dirname(Path),
    NewPath = filename:join(Dir, <<NewName/binary, ".erl">>),
    NewUri = els_uri:uri(NewPath),
    %% Find references that needs to be changed
    Refs = els_references_provider:find_references_to_module(OldUri),
    RefPOIs = convert_references_to_pois(Refs, [
        application,
        implicit_fun,
        import_entry,
        type_application,
        behaviour
    ]),
    Changes = [
        #{
            textDocument =>
                #{uri => RefUri, version => null},
            edits => [
                #{
                    range => editable_range(RefPOI, module),
                    newText => NewName
                }
            ]
        }
     || {RefUri, RefPOI} <- RefPOIs
    ],
    #{
        documentChanges =>
            %% Update -module attribute
            [
                #{
                    textDocument => #{uri => OldUri, version => null},
                    edits => [change(POI, NewName)]
                },
                %% Rename file
                #{kind => rename, oldUri => OldUri, newUri => NewUri}
                | Changes
            ]
    };
workspace_edits(Uri, [#{kind := function_clause} = POI | _], NewName) ->
    #{id := {F, A, _}} = POI,
    #{changes => changes(Uri, POI#{kind => function, id => {F, A}}, NewName)};
workspace_edits(Uri, [#{kind := spec} = POI | _], NewName) ->
    #{changes => changes(Uri, POI#{kind => function}, NewName)};
workspace_edits(Uri, [#{kind := Kind} = POI | _], NewName) when
    Kind =:= define;
    Kind =:= record;
    Kind =:= record_def_field;
    Kind =:= function;
    Kind =:= type_definition;
    Kind =:= variable
->
    #{changes => changes(Uri, POI, NewName)};
workspace_edits(Uri, [#{kind := Kind} = POI | _], NewName) when
    Kind =:= macro;
    Kind =:= record_expr;
    Kind =:= record_field;
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= export_entry;
    Kind =:= import_entry;
    Kind =:= export_type_entry;
    Kind =:= type_application
->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, [{DefUri, DefPOI}]} ->
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
                Acc#{
                    U =>
                        [
                            #{
                                range => editable_range(P),
                                newText => NewName
                            }
                         || #{id := I} = P <- ExportEntries, I =:= Id
                        ] ++
                        [
                            #{
                                range => editable_range(P),
                                newText => NewName
                            }
                         || #{id := {N, A, _I}} = P <- FunctionClauses,
                            N =:= Name,
                            A =:= Arity
                        ] ++
                        [
                            #{
                                range => editable_range(P),
                                newText => NewName
                            }
                         || #{id := I} = P <- Specs, I =:= Id
                        ]
                }
            end,
            #{
                Uri =>
                    [
                        #{
                            range => editable_range(POI),
                            newText => NewName
                        }
                    ]
            },
            Refs
        ),
    #{changes => Changes};
workspace_edits(_Uri, _POIs, _NewName) ->
    null.

-spec editable_range(els_poi:poi()) -> range().
editable_range(POI) ->
    editable_range(POI, function).

-spec editable_range(els_poi:poi(), function | module) -> range().
editable_range(#{kind := Kind, data := #{mod_range := Range}}, module) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= import_entry;
    Kind =:= type_application;
    Kind =:= behaviour
->
    els_protocol:range(Range);
editable_range(#{kind := Kind, data := #{name_range := Range}}, function) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= callback;
    Kind =:= spec;
    Kind =:= export_entry;
    Kind =:= export_type_entry;
    Kind =:= import_entry;
    Kind =:= type_application;
    Kind =:= type_definition
->
    %% application POI of a local call and
    %% type_application POI of a built-in type don't have name_range data
    %% they are handled by the next clause
    els_protocol:range(Range);
editable_range(#{kind := _Kind, range := Range}, _) ->
    els_protocol:range(Range).

-spec changes(uri(), els_poi:poi(), binary()) -> #{uri() => [text_edit()]} | null.
changes(Uri, #{kind := module} = Mod, NewName) ->
    #{Uri => [#{range => editable_range(Mod), newText => NewName}]};
changes(Uri, #{kind := variable} = Var, NewName) ->
    POIs = els_code_navigation:find_in_scope(Uri, Var),
    #{Uri => [#{range => editable_range(P), newText => NewName} || P <- POIs]};
changes(Uri, #{kind := type_definition, id := {Name, A}}, NewName) ->
    ?LOG_INFO("Renaming type ~p/~p to ~s", [Name, A, NewName]),
    {ok, Doc} = els_utils:lookup_document(Uri),
    SelfChanges = [
        change(P, NewName)
     || P <- els_dt_document:pois(Doc, [
            type_definition,
            export_type_entry
        ]),
        maps:get(id, P) =:= {Name, A}
    ],
    Key = {els_uri:module(Uri), Name, A},
    {ok, Refs} = els_dt_references:find_by_id(type_application, Key),
    RefPOIs = convert_references_to_pois(Refs, [type_application]),
    Changes =
        lists:foldl(
            fun({RefUri, RefPOI}, Acc) ->
                Change = change(RefPOI, NewName),
                maps:update_with(RefUri, fun(V) -> [Change | V] end, [Change], Acc)
            end,
            #{Uri => SelfChanges},
            RefPOIs
        ),
    ?LOG_INFO(
        "Done renaming type ~p/~p to ~s. ~p changes in ~p files.",
        [
            Name,
            A,
            NewName,
            length(lists:flatten(maps:values(Changes))),
            length(maps:keys(Changes))
        ]
    ),
    Changes;
changes(Uri, #{kind := function, id := {F, A}}, NewName) ->
    ?LOG_INFO("Renaming function ~p/~p to ~s", [F, A, NewName]),
    {ok, Doc} = els_utils:lookup_document(Uri),
    IsMatch = fun
        (#{id := {Fun, Arity}}) -> {Fun, Arity} =:= {F, A};
        (#{id := {Fun, Arity, _}}) -> {Fun, Arity} =:= {F, A};
        (_) -> false
    end,
    SelfChanges = [
        change(P, NewName)
     || P <- els_dt_document:pois(Doc, [
            export_entry,
            function_clause,
            spec
        ]),
        IsMatch(P)
    ],
    Key = {els_uri:module(Uri), F, A},
    {ok, Refs} = els_dt_references:find_by_id(function, Key),
    RefPOIs = convert_references_to_pois(Refs, [
        application,
        implicit_fun,
        import_entry
    ]),
    Changes =
        lists:foldl(
            fun({RefUri, RefPOI}, Acc) ->
                ImportChanges = import_changes(RefUri, RefPOI, NewName),
                Changes = [change(RefPOI, NewName)] ++ ImportChanges,
                maps:update_with(RefUri, fun(V) -> Changes ++ V end, Changes, Acc)
            end,
            #{Uri => SelfChanges},
            RefPOIs
        ),
    ?LOG_INFO(
        "Done renaming function ~p/~p to ~s. ~p changes in ~p files.",
        [
            F,
            A,
            NewName,
            length(lists:flatten(maps:values(Changes))),
            length(maps:keys(Changes))
        ]
    ),
    Changes;
changes(Uri, #{kind := DefKind} = DefPOI, NewName) when
    DefKind =:= define;
    DefKind =:= record;
    DefKind =:= record_def_field
->
    Self = #{range => editable_range(DefPOI), newText => NewName},
    Refs = els_references_provider:find_scoped_references_for_def(Uri, DefPOI),
    lists:foldl(
        fun(#{uri := U, range := R}, Acc) ->
            Change = #{
                range => R,
                newText => new_name(DefKind, NewName)
            },
            maps:update_with(U, fun(V) -> [Change | V] end, [Change], Acc)
        end,
        #{Uri => [Self]},
        Refs
    );
changes(_Uri, _POI, _NewName) ->
    null.

-spec new_name(els_poi:poi_kind(), binary()) -> binary().
new_name(define, NewName) ->
    <<"?", NewName/binary>>;
new_name(record, NewName) ->
    <<"#", NewName/binary>>;
new_name(_, NewName) ->
    NewName.

-spec convert_references_to_pois([els_dt_references:item()], [els_poi:poi_kind()]) ->
    [{uri(), els_poi:poi()}].
convert_references_to_pois(Refs, Kinds) ->
    UriPOIs = lists:foldl(
        fun
            (#{uri := Uri}, Acc) when is_map_key(Uri, Acc) ->
                Acc;
            (#{uri := Uri}, Acc) ->
                POIs =
                    case els_utils:lookup_document(Uri) of
                        {ok, Doc} ->
                            els_dt_document:pois(Doc, Kinds);
                        {error, _} ->
                            []
                    end,
                maps:put(Uri, POIs, Acc)
        end,
        #{},
        Refs
    ),
    lists:map(
        fun(#{uri := Uri, range := #{from := Pos}}) ->
            POIs = maps:get(Uri, UriPOIs),
            {Uri, hd(els_poi:match_pos(POIs, Pos))}
        end,
        Refs
    ).

%% @doc Find all uses of imported function in Uri
-spec import_changes(uri(), els_poi:poi(), binary()) -> [text_edit()].
import_changes(Uri, #{kind := import_entry, id := {_M, F, A}}, NewName) ->
    case els_utils:lookup_document(Uri) of
        {ok, Doc} ->
            [
                change(P, NewName)
             || P <- els_dt_document:pois(Doc, [
                    application,
                    implicit_fun
                ]),
                maps:get(id, P) =:= {F, A}
            ];
        {error, _} ->
            []
    end;
import_changes(_Uri, _POI, _NewName) ->
    [].

-spec change(els_poi:poi(), binary()) -> text_edit().
change(POI, NewName) ->
    #{range => editable_range(POI), newText => NewName}.
