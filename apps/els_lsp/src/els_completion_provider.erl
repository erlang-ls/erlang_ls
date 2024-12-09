-module(els_completion_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    handle_request/1,
    trigger_characters/0,
    bif_pois/1
]).

%% Exported to ease testing.
-export([
    bifs/2,
    keywords/2
]).

-type options() :: #{
    trigger := binary(),
    document := els_dt_document:item(),
    line := line(),
    column := column()
}.

-type items() :: [item()].
-type item() :: completion_item().

-type item_format() :: arity_only | args | no_args.
-type tokens() :: [any()].
-type poi_kind_or_any() :: els_poi:poi_kind() | any.

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec trigger_characters() -> [binary()].
trigger_characters() ->
    [
        <<":">>,
        <<"#">>,
        <<"?">>,
        <<".">>,
        <<"-">>,
        <<"\"">>,
        <<"{">>,
        <<"/">>,
        <<" ">>
    ].

-spec handle_request(els_provider:provider_request()) ->
    {response, any()} | {async, uri(), pid()}.
handle_request({completion, Params}) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    Context = maps:get(
        <<"context">>,
        Params,
        #{<<"triggerKind">> => ?COMPLETION_TRIGGER_KIND_INVOKED}
    ),
    TriggerKind = maps:get(<<"triggerKind">>, Context),
    TriggerCharacter = maps:get(<<"triggerCharacter">>, Context, <<>>),
    Job = run_completion_job(Uri, Line, Character, TriggerKind, TriggerCharacter),
    {async, Uri, Job};
handle_request({resolve, CompletionItem}) ->
    {response, resolve(CompletionItem)}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec run_completion_job(
    uri(),
    line(),
    column(),
    completion_trigger_kind(),
    binary()
) -> pid().
run_completion_job(Uri, Line, Character, TriggerKind, TriggerCharacter) ->
    {ok, #{text := Text} = Document} = els_utils:lookup_document(Uri),
    %% We subtract 1 to strip the character that triggered the
    %% completion from the string.
    Length =
        case Character > 0 of
            true -> 1;
            false -> 0
        end,
    Prefix =
        case TriggerKind of
            ?COMPLETION_TRIGGER_KIND_CHARACTER ->
                els_text:line(Text, Line, Character - Length);
            ?COMPLETION_TRIGGER_KIND_INVOKED ->
                els_text:line(Text, Line, Character);
            ?COMPLETION_TRIGGER_KIND_FOR_INCOMPLETE_COMPLETIONS ->
                els_text:line(Text, Line, Character)
        end,
    ?LOG_INFO("Find completions for ~s", [Prefix]),
    Opts = #{
        trigger => TriggerCharacter,
        document => Document,
        line => Line + 1,
        column => Character
    },
    Config = #{
        task => fun find_completions/2,
        entries => [{Prefix, TriggerKind, Opts}],
        title => <<"Completion">>,
        on_complete => fun els_server:register_result/1
    },
    {ok, Pid} = els_background_job:new(Config),
    Pid.

-spec find_completions({binary(), completion_trigger_kind(), options()}, any()) -> items().
find_completions({Prefix, TriggerKind, Opts}, _) ->
    Result = find_completions(Prefix, TriggerKind, Opts),
    ?LOG_INFO("Found completions: ~p", [length(Result)]),
    Result.

-spec resolve(map()) -> map().
resolve(
    #{
        <<"kind">> := ?COMPLETION_ITEM_KIND_FUNCTION,
        <<"data">> := #{
            <<"module">> := Module,
            <<"function">> := Function,
            <<"arity">> := Arity
        }
    } = CompletionItem
) ->
    Entries = els_docs:function_docs(
        'remote',
        binary_to_atom(Module, utf8),
        binary_to_atom(Function, utf8),
        Arity
    ),
    CompletionItem#{documentation => els_markup_content:new(Entries)};
resolve(
    #{
        <<"kind">> := ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"data">> := #{
            <<"module">> := Module,
            <<"type">> := Type,
            <<"arity">> := Arity
        }
    } = CompletionItem
) ->
    Entries = els_docs:type_docs(
        'remote',
        binary_to_atom(Module, utf8),
        binary_to_atom(Type, utf8),
        Arity
    ),
    CompletionItem#{documentation => els_markup_content:new(Entries)};
resolve(CompletionItem) ->
    CompletionItem.

-spec find_completions(binary(), completion_trigger_kind(), options()) -> items().
find_completions(
    Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{
        trigger := <<":">>,
        document := Document,
        line := Line,
        column := Column
    }
) ->
    case lists:reverse(els_text:tokens(Prefix)) of
        [{atom, _, Module}, {'fun', _} | _] ->
            exported_definitions(Module, 'function', arity_only);
        [{atom, _, Module} | _] = Tokens ->
            {ItemFormat, TypeOrFun} =
                completion_context(Document, Line, Column, Tokens),
            exported_definitions(Module, TypeOrFun, ItemFormat);
        [{var, _, 'MODULE'}, {'?', _}, {'fun', _} | _] ->
            Module = els_uri:module(els_dt_document:uri(Document)),
            exported_definitions(Module, 'function', arity_only);
        [{var, _, 'MODULE'}, {'?', _} | _] = Tokens ->
            Module = els_uri:module(els_dt_document:uri(Document)),
            {ItemFormat, TypeOrFun} =
                completion_context(Document, Line, Column, Tokens),
            exported_definitions(Module, TypeOrFun, ItemFormat);
        _ ->
            []
    end;
find_completions(
    _Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"?">>, document := Document}
) ->
    bifs(define, _ItemFormat = args) ++ definitions(Document, define);
find_completions(
    _Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"-">>, document := Document, column := 1, line := Line}
) ->
    attributes(Document, Line);
find_completions(
    Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"/">>}
) ->
    Tokens = lists:reverse(els_text:tokens(Prefix)),
    case in_binary_heuristic(Tokens) of
        true ->
            binary_type_specifier();
        false ->
            []
    end;
find_completions(
    Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"-">>}
) ->
    binary_type_specifiers(Prefix);
find_completions(
    _Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"#">>, document := Document}
) ->
    definitions(Document, record);
find_completions(
    Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"{">>, document := Document}
) ->
    case lists:reverse(els_text:tokens(string:trim(Prefix))) of
        [{atom, _, Name} | _] ->
            record_fields_with_var(Document, Name);
        _ ->
            []
    end;
find_completions(
    Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<" ">>} = Opts
) ->
    case lists:reverse(els_text:tokens(string:trim(Prefix))) of
        [{',', _} | _] = Tokens ->
            complete_record_field(Opts, Tokens);
        _ ->
            []
    end;
find_completions(
    <<"-include_lib(">>,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"\"">>}
) ->
    [item_kind_file(Path) || Path <- els_include_paths:include_libs()];
find_completions(
    <<"-include(">>,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"\"">>, document := Document}
) ->
    [item_kind_file(Path) || Path <- els_include_paths:includes(Document)];
find_completions(
    Prefix,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<".">>, document := Document}
) ->
    case lists:reverse(els_text:tokens(Prefix)) of
        [{atom, _, RecordName}, {'#', _} | _] ->
            record_fields(Document, RecordName);
        _ ->
            []
    end;
find_completions(
    Prefix,
    TriggerKind,
    #{
        document := Document,
        line := Line,
        column := Column
    } = Opts
) when
    TriggerKind =:= ?COMPLETION_TRIGGER_KIND_INVOKED;
    TriggerKind =:= ?COMPLETION_TRIGGER_KIND_FOR_INCOMPLETE_COMPLETIONS
->
    case lists:reverse(els_text:tokens(Prefix)) of
        %% Check for "[...] fun atom:"
        [{':', _}, {atom, _, Module}, {'fun', _} | _] ->
            exported_definitions(Module, function, arity_only);
        %% Check for "[...] fun atom:atom"
        [{atom, _, _}, {':', _}, {atom, _, Module}, {'fun', _} | _] ->
            exported_definitions(Module, function, arity_only);
        %% Check for "[...] atom:"
        [{':', _}, {atom, _, Module} | _] = Tokens ->
            {ItemFormat, POIKind} =
                completion_context(Document, Line, Column, Tokens),
            exported_definitions(Module, POIKind, ItemFormat);
        %% Check for "[...] atom:atom"
        [{atom, _, _}, {':', _}, {atom, _, Module} | _] = Tokens ->
            {ItemFormat, POIKind} =
                completion_context(Document, Line, Column, Tokens),
            exported_definitions(Module, POIKind, ItemFormat);
        %% Check for "[...] ?"
        [{'?', _} | _] ->
            bifs(define, _ItemFormat = args) ++ definitions(Document, define);
        %% Check for "[...] ?anything"
        [_, {'?', _} | _] ->
            bifs(define, _ItemFormat = args) ++ definitions(Document, define);
        %% Check for "[...] #anything."
        [{'.', _}, {atom, _, RecordName}, {'#', _} | _] ->
            record_fields(Document, RecordName);
        %% Check for "[...] #anything.something"
        [_, {'.', _}, {atom, _, RecordName}, {'#', _} | _] ->
            record_fields(Document, RecordName);
        %% Check for "[...] #"
        [{'#', _} | _] ->
            definitions(Document, record);
        %% Check for "#{"
        [{'{', _}, {'#', _} | _] ->
            [map_comprehension_completion_item(Document, Line, Column)];
        %% Check for "[...] #anything"
        [_, {'#', _} | _] ->
            definitions(Document, record);
        %% Check for "[...] #anything{"
        [{'{', _}, {atom, _, RecordName}, {'#', _} | _] ->
            record_fields_with_var(Document, RecordName);
        %% Check for "[...] Variable"
        [{var, _, _} | _] ->
            variables(Document);
        %% Check for "-anything"
        [{atom, _, _}, {'-', _}] ->
            attributes(Document, Line);
        %% Check for "[...] -"
        [{'-', _} | _] ->
            binary_type_specifiers(Prefix);
        %% Check for "[...] -"
        [{'/', _} | _] ->
            Tokens = lists:reverse(els_text:tokens(Prefix)),
            case in_binary_heuristic(Tokens) of
                true ->
                    binary_type_specifier();
                false ->
                    []
            end;
        %% Check for "-export(["
        [{'[', _}, {'(', _}, {atom, _, export}, {'-', _}] ->
            unexported_definitions(Document, function);
        %% Check for "-nifs(["
        [{'[', _}, {'(', _}, {atom, _, nifs}, {'-', _}] ->
            definitions(Document, function, arity_only, false);
        %% Check for "-export_type(["
        [{'[', _}, {'(', _}, {atom, _, export_type}, {'-', _}] ->
            unexported_definitions(Document, type_definition);
        %% Check for "-feature("
        [{'(', _}, {atom, _, feature}, {'-', _}] ->
            features();
        %% Check for "?FEATURE_ENABLED("
        [{'(', _}, {var, _, 'FEATURE_ENABLED'}, {'?', _} | _] ->
            features();
        %% Check for "?FEATURE_AVAILABLE("
        [{'(', _}, {var, _, 'FEATURE_AVAILABLE'}, {'?', _} | _] ->
            features();
        %% Check for "-behaviour(anything"
        [{atom, _, Begin}, {'(', _}, {atom, _, Attribute}, {'-', _}] when
            Attribute =:= behaviour; Attribute =:= behavior
        ->
            [
                item_kind_module(Module)
             || Module <- behaviour_modules(atom_to_list(Begin))
            ];
        %% Check for "-behaviour("
        [{'(', _}, {atom, _, Attribute}, {'-', _}] when
            Attribute =:= behaviour; Attribute =:= behavior
        ->
            [item_kind_module(Module) || Module <- behaviour_modules("")];
        %% Check for "["
        [{'[', _} | _] ->
            [list_comprehension_completion_item(Document, Line, Column)];
        %% Check for "[...] fun atom"
        [{atom, _, _}, {'fun', _} | _] ->
            bifs(function, ItemFormat = arity_only) ++
                definitions(Document, function, ItemFormat = arity_only);
        %% Check for "| atom"
        [{atom, _, Name}, {'|', _} | _] = Tokens ->
            {ItemFormat, _POIKind} =
                completion_context(Document, Line, Column, Tokens),
            complete_type_definition(Document, Name, ItemFormat);
        %% Check for "::"
        [{'::', _} | _] = Tokens ->
            {ItemFormat, _POIKind} =
                completion_context(Document, Line, Column, Tokens),
            complete_type_definition(Document, '', ItemFormat);
        %% Check for ":: atom"
        [{atom, _, Name}, {'::', _} | _] = Tokens ->
            {ItemFormat, _POIKind} =
                completion_context(Document, Line, Column, Tokens),
            complete_type_definition(Document, Name, ItemFormat);
        %% Check for "[...] atom"
        [{atom, _, Name} | _] = Tokens ->
            complete_atom(Name, Tokens, Opts);
        %% Treat keywords as atom completion
        [{Name, _} | _] = Tokens ->
            case lists:member(Name, keywords()) of
                true ->
                    complete_atom(Name, Tokens, Opts);
                false ->
                    []
            end;
        Tokens ->
            ?LOG_DEBUG(
                "No completion found. [prefix=~p] [tokens=~p]",
                [Prefix, Tokens]
            ),
            []
    end;
find_completions(_Prefix, _TriggerKind, _Opts) ->
    [].

-spec list_comprehension_completion_item(els_dt_document:item(), line(), column()) ->
    completion_item().
list_comprehension_completion_item(#{text := Text}, Line, Column) ->
    Suffix =
        try els_text:get_char(Text, Line, Column + 1) of
            {ok, $]} ->
                %% Don't include ']' if next character is a ']'
                %% I.e if cursor is at []
                %%                     ^
                <<"">>;
            _ ->
                <<"]">>
        catch
            _:_:_ ->
                <<"]">>
        end,
    InsertText =
        case snippet_support() of
            true ->
                <<"${3:Expr} || ${2:Elem} <- ${1:List}", Suffix/binary>>;
            false ->
                <<"Expr || Elem <- List", Suffix/binary>>
        end,
    #{
        label => <<"[Expr || Elem <- List]">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText => InsertText
    }.

-spec map_comprehension_completion_item(els_dt_document:item(), line(), column()) ->
    completion_item().
map_comprehension_completion_item(#{text := Text}, Line, Column) ->
    Suffix =
        try els_text:get_char(Text, Line, Column + 1) of
            {ok, $}} ->
                %% Don't include '}' if next character is a '}'
                %% I.e if cursor is at #{}
                %%                      ^
                <<"">>;
            _ ->
                <<"}">>
        catch
            _:_:_ ->
                <<"}">>
        end,
    InsertText =
        case snippet_support() of
            true ->
                <<"${4:K} => ${5:V} || ${2:K} => ${3:V} <- ${1:Map}", Suffix/binary>>;
            false ->
                <<"K => V || K := V <- Map", Suffix/binary>>
        end,
    #{
        label => <<"#{K => V || K := V <- Map}">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText => InsertText
    }.

-spec complete_atom(atom(), [any()], map()) -> [completion_item()].
complete_atom(Name, Tokens, Opts) ->
    #{document := Document, line := Line, column := Column} = Opts,
    NameBinary = atom_to_binary(Name, utf8),
    {ItemFormat, POIKind} = completion_context(Document, Line, Column, Tokens),
    case ItemFormat of
        arity_only ->
            #{text := Text} = Document,
            case
                is_in(Document, Line, Column, [nifs]) orelse
                    is_in_heuristic(Text, <<"nifs">>, Line - 1)
            of
                true ->
                    definitions(Document, POIKind, ItemFormat, false);
                _ ->
                    %% Only complete unexported definitions when in
                    %% export
                    unexported_definitions(Document, POIKind)
            end;
        _ ->
            case complete_record_field(Opts, Tokens) of
                [] ->
                    keywords(POIKind, ItemFormat) ++
                        bifs(POIKind, ItemFormat) ++
                        atoms(Document, NameBinary) ++
                        all_record_fields(Document, NameBinary) ++
                        modules(NameBinary) ++
                        definitions(Document, POIKind, ItemFormat) ++
                        snippets(POIKind, ItemFormat);
                RecordFields ->
                    RecordFields
            end
    end.

-spec binary_type_specifiers(binary()) -> [completion_item()].
binary_type_specifiers(Prefix) ->
    %% in_binary_heuristic will only consider current line
    %% TODO: make it work for multi-line binaries too.
    Tokens = lists:reverse(els_text:tokens(Prefix)),
    case
        in_binary_heuristic(Tokens) andalso
            in_binary_type_specifier(Tokens, [])
    of
        {true, TypeListTokens} ->
            HasType = lists:any(
                fun(T) ->
                    lists:member(T, binary_types())
                end,
                TypeListTokens
            ),
            HasEndianess = lists:any(
                fun(T) ->
                    lists:member(T, binary_endianness())
                end,
                TypeListTokens
            ),
            HasSignedness = lists:any(
                fun(T) ->
                    lists:member(T, binary_signedness())
                end,
                TypeListTokens
            ),
            HasUnit = lists:member(unit, TypeListTokens),
            [binary_type_specifier(unit) || not HasUnit] ++
                [binary_type_specifier(Label) || Label <- binary_types(), not HasType] ++
                [binary_type_specifier(Label) || Label <- binary_endianness(), not HasEndianess] ++
                [binary_type_specifier(Label) || Label <- binary_signedness(), not HasSignedness];
        false ->
            []
    end.

-spec in_binary_heuristic([any()]) -> boolean().
in_binary_heuristic([{'>>', _} | _]) ->
    false;
in_binary_heuristic([{'<<', _} | _]) ->
    true;
in_binary_heuristic([_ | T]) ->
    in_binary_heuristic(T);
in_binary_heuristic([]) ->
    false.

-spec in_binary_type_specifier([any()], [atom()]) -> {true, [atom()]} | false.
in_binary_type_specifier([{integer, _, _}, {':', _}, {atom, _, unit} | T], Spec) ->
    in_binary_type_specifier(T, [unit | Spec]);
in_binary_type_specifier([{atom, _, Atom} | T], Spec) ->
    case lists:member(Atom, binary_type_specifiers()) of
        true ->
            in_binary_type_specifier(T, [Atom | Spec]);
        false ->
            false
    end;
in_binary_type_specifier([{'-', _} | T], Spec) ->
    in_binary_type_specifier(T, Spec);
in_binary_type_specifier([{'/', _} | _], Spec) ->
    {true, Spec};
in_binary_type_specifier([], _Spec) ->
    false.

-spec binary_type_specifiers() -> [atom()].
binary_type_specifiers() ->
    binary_types() ++ binary_signedness() ++ binary_endianness() ++ [unit].

-spec binary_signedness() -> [atom()].
binary_signedness() ->
    [signed, unsigned].

-spec binary_types() -> [atom()].
binary_types() ->
    [integer, float, binary, bytes, bitstring, bits, utf8, utf16, utf32].

-spec binary_endianness() -> [atom()].
binary_endianness() ->
    [big, little, native].

-spec binary_type_specifier() -> [completion_item()].
binary_type_specifier() ->
    Labels = binary_type_specifiers(),
    [binary_type_specifier(Label) || Label <- Labels].

-spec binary_type_specifier(atom()) -> completion_item().
binary_type_specifier(unit) ->
    case snippet_support() of
        true ->
            #{
                label => <<"unit:N">>,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                insertText => <<"unit:${1:N}">>
            };
        false ->
            #{
                label => <<"unit:">>,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
            }
    end;
binary_type_specifier(Label) ->
    #{
        label => atom_to_binary(Label),
        kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
    }.

-spec complete_record_field(map(), list()) -> items().
complete_record_field(_Opts, [{atom, _, _}, {'=', _} | _]) ->
    [];
complete_record_field(
    #{document := Document, line := Line, column := Col},
    _Tokens
) ->
    complete_record_field(Document, {Line, Col}, <<"key=val}.">>).

-spec complete_record_field(map(), pos(), binary()) -> items().
complete_record_field(#{text := Text0} = Document, Pos, Suffix) ->
    Prefix0 = els_text:range(Text0, {1, 1}, Pos),
    POIs = els_dt_document:pois(Document, [function, spec, define, callback, record]),
    %% Look for record start between current position and end of last
    %% relevant top level expression
    Prefix =
        case els_scope:pois_before(POIs, #{from => Pos, to => Pos}) of
            [#{range := #{to := {Line, _}}} | _] ->
                {_, Prefix1} = els_text:split_at_line(Prefix0, Line),
                Prefix1;
            _ ->
                %% Found no POI before, consider all the text
                Prefix0
        end,
    case parse_record(els_text:strip_comments(Prefix), Suffix) of
        {ok, Id} ->
            record_fields_with_var(Document, Id);
        error ->
            []
    end.

-spec parse_record(binary(), binary()) -> {ok, els_poi:poi_id()} | error.
parse_record(Text, Suffix) ->
    case string:split(Text, <<"#">>, trailing) of
        [_] ->
            error;
        [Left, Right] ->
            Str = <<"#", Right/binary, Suffix/binary>>,
            case els_parser:parse(Str) of
                {ok, [#{kind := record_expr, id := Id} | _]} ->
                    {ok, Id};
                _ ->
                    parse_record(Left, Str)
            end
    end.

-spec snippets(poi_kind_or_any(), item_format()) -> items().
snippets(type_definition, _ItemFormat) ->
    [];
snippets(_POIKind, args) ->
    els_snippets_server:snippets();
snippets(_POIKind, _ItemFormat) ->
    [].

-spec poikind_from_tokens(tokens()) -> poi_kind_or_any().
poikind_from_tokens(Tokens) ->
    case Tokens of
        [{'::', _} | _] ->
            type_definition;
        [{atom, _, _}, {'::', _} | _] ->
            type_definition;
        [{atom, _, _}, {'|', _} | _] ->
            type_definition;
        [{atom, _, _}, {'=', _} | _] ->
            function;
        _ ->
            any
    end.

-spec complete_type_definition(els_dt_document:item(), atom(), item_format()) -> items().
complete_type_definition(Document, Name, ItemFormat) ->
    NameBinary = atom_to_binary(Name, utf8),
    definitions(Document, type_definition, ItemFormat) ++
        bifs(type_definition, ItemFormat) ++
        modules(NameBinary) ++
        atoms(Document, NameBinary).

%%=============================================================================
%% Attributes
%%=============================================================================
-spec attributes(els_dt_document:item(), line()) -> items().
attributes(Document, Line) ->
    [
        snippet(attribute_behaviour),
        snippet(attribute_callback),
        snippet(attribute_compile),
        snippet(attribute_define),
        snippet(attribute_dialyzer),
        snippet(attribute_export),
        snippet(attribute_export_type),
        snippet(attribute_feature),
        snippet(attribute_if),
        snippet(attribute_ifdef),
        snippet(attribute_ifndef),
        snippet(attribute_import),
        snippet(attribute_include),
        snippet(attribute_include_lib),
        snippet(attribute_on_load),
        snippet(attribute_nifs),
        snippet(attribute_opaque),
        snippet(attribute_record),
        snippet(attribute_type),
        snippet(attribute_vsn),
        attribute_module(Document)
    ] ++ docs_attributes() ++ attribute_spec(Document, Line).

-spec attribute_module(els_dt_document:item()) -> item().
attribute_module(#{id := Id}) ->
    IdBin = atom_to_binary(Id, utf8),
    snippet(
        <<"-module(", IdBin/binary, ").">>,
        <<"module(", IdBin/binary, ").">>
    ).

-spec docs_attributes() -> items().
-if(?OTP_RELEASE >= 27).
docs_attributes() ->
    [
        snippet(attribute_moduledoc_map),
        snippet(attribute_doc_map),
        snippet(attribute_moduledoc_file),
        snippet(attribute_doc_file),
        snippet(attribute_moduledoc_text),
        snippet(attribute_doc_text),
        snippet(attribute_moduledoc_false),
        snippet(attribute_doc_false)
    ].
-else.
docs_attributes() ->
    [].
-endif.

-spec attribute_spec(Document :: els_dt_document:item(), line()) -> items().
attribute_spec(#{text := Text}, Line) ->
    POIs = els_incomplete_parser:parse_after(Text, Line),
    case [P || #{kind := function} = P <- POIs] of
        [] ->
            [];
        FunPOIs ->
            [#{id := {Id, Arity}} | _] = els_poi:sort(FunPOIs),
            Args = [els_arg:new(I, "_") || I <- lists:seq(1, Arity)],
            SnippetSupport = snippet_support(),
            FunBin = format_function(Id, Args, SnippetSupport, spec),
            RetBin =
                case SnippetSupport of
                    false ->
                        <<" -> _.">>;
                    true ->
                        N = integer_to_binary(Arity + 1),
                        <<" -> ${", N/binary, ":_}.">>
                end,
            [snippet(<<"-spec">>, <<"spec ", FunBin/binary, RetBin/binary>>)]
    end.

%%=============================================================================
%% Include paths
%%=============================================================================
-spec item_kind_file(binary()) -> item().
item_kind_file(Path) ->
    #{
        label => Path,
        kind => ?COMPLETION_ITEM_KIND_FILE,
        insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
    }.

%%==============================================================================
%% Snippets
%%==============================================================================
-spec snippet(atom()) -> item().
snippet(attribute_behaviour) ->
    snippet(<<"-behaviour().">>, <<"behaviour(${1:Behaviour}).">>);
snippet(attribute_export) ->
    snippet(<<"-export().">>, <<"export([${1:}]).">>);
snippet(attribute_vsn) ->
    snippet(<<"-vsn(Version).">>, <<"vsn(${1:Version}).">>);
snippet(attribute_callback) ->
    snippet(
        <<"-callback name(Args) -> return().">>,
        <<"callback ${1:name}(${2:Args}) -> ${3:return()}.">>
    );
snippet(attribute_on_load) ->
    snippet(
        <<"-on_load().">>,
        <<"on_load(${1:Function}).">>
    );
snippet(attribute_nifs) ->
    snippet(
        <<"-nifs().">>,
        <<"nifs([${1:}]).">>
    );
snippet(attribute_export_type) ->
    snippet(<<"-export_type().">>, <<"export_type([${1:}]).">>);
snippet(attribute_feature) ->
    snippet(<<"-feature().">>, <<"feature(${1:Feature}, ${2:enable}).">>);
snippet(attribute_include) ->
    snippet(<<"-include().">>, <<"include(${1:}).">>);
snippet(attribute_include_lib) ->
    snippet(<<"-include_lib().">>, <<"include_lib(${1:}).">>);
snippet(attribute_type) ->
    snippet(
        <<"-type name() :: definition.">>,
        <<"type ${1:name}() :: ${2:definition}.">>
    );
snippet(attribute_opaque) ->
    snippet(
        <<"-opaque name() :: definition.">>,
        <<"opaque ${1:name}() :: ${2:definition}.">>
    );
snippet(attribute_ifdef) ->
    snippet(<<"-ifdef().">>, <<"ifdef(${1:VAR}).\n${2:}\n-endif.">>);
snippet(attribute_ifndef) ->
    snippet(<<"-ifndef().">>, <<"ifndef(${1:VAR}).\n${2:}\n-endif.">>);
snippet(attribute_if) ->
    snippet(<<"-if().">>, <<"if(${1:Pred}).\n${2:}\n-endif.">>);
snippet(attribute_define) ->
    snippet(<<"-define().">>, <<"define(${1:MACRO}, ${2:Value}).">>);
snippet(attribute_record) ->
    snippet(
        <<"-record().">>,
        <<"record(${1:name}, {${2:field} = ${3:Value} :: ${4:Type}()}).">>
    );
snippet(attribute_import) ->
    snippet(
        <<"-import().">>,
        <<"import(${1:Module}, [${2:}]).">>
    );
snippet(attribute_dialyzer) ->
    snippet(
        <<"-dialyzer().">>,
        <<"dialyzer(${1:}).">>
    );
snippet(attribute_compile) ->
    snippet(
        <<"-compile().">>,
        <<"compile(${1:}).">>
    );
snippet(attribute_moduledoc_text) ->
    snippet(
        <<"-moduledoc \"\"\"Text\"\"\".">>,
        <<"moduledoc \"\"\"\n${1:Text}\n\"\"\".">>
    );
snippet(attribute_doc_text) ->
    snippet(
        <<"-doc \"\"\"Text\"\"\".">>,
        <<"doc \"\"\"\n${1:Text}\n\"\"\".">>
    );
snippet(attribute_moduledoc_false) ->
    snippet(
        <<"-moduledoc false.">>,
        <<"moduledoc false.">>
    );
snippet(attribute_doc_false) ->
    snippet(
        <<"-doc false.">>,
        <<"doc false.">>
    );
snippet(attribute_moduledoc_map) ->
    snippet(
        <<"-moduledoc #{}.">>,
        <<"moduledoc #{${1:}}.">>
    );
snippet(attribute_doc_map) ->
    snippet(
        <<"-doc #{}.">>,
        <<"doc #{${1:}}.">>
    );
snippet(attribute_moduledoc_file) ->
    snippet(
        <<"-moduledoc File.">>,
        <<"moduledoc {file,\"${1:File}\"}.">>
    );
snippet(attribute_doc_file) ->
    snippet(
        <<"-doc File.">>,
        <<"doc {file,\"${1:File}\"}.">>
    ).

-spec snippet(binary(), binary()) -> item().
snippet(Label, InsertText) ->
    #{
        label => Label,
        kind => ?COMPLETION_ITEM_KIND_SNIPPET,
        insertText => InsertText,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
    }.

%%==============================================================================
%% Atoms
%%==============================================================================

-spec atoms(els_dt_document:item(), binary()) -> [map()].
atoms(Document, Prefix) ->
    POIs = els_scope:local_and_included_pois(Document, atom),
    Atoms = [Id || #{id := Id} <- POIs],
    Unique = lists:usort(Atoms),
    filter_by_prefix(Prefix, Unique, fun atom_to_label/1, fun item_kind_atom/1).

-spec item_kind_atom(binary()) -> map().
item_kind_atom(Atom) ->
    #{
        label => Atom,
        kind => ?COMPLETION_ITEM_KIND_CONSTANT,
        insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
    }.

%%==============================================================================
%% Modules
%%==============================================================================

-spec modules(binary()) -> [map()].
modules(Prefix) ->
    {ok, Items} = els_dt_document_index:find_by_kind(module),
    Modules = [Id || #{id := Id} <- Items],
    filter_by_prefix(
        Prefix,
        Modules,
        fun atom_to_label/1,
        fun item_kind_module/1
    ).

-spec item_kind_module(atom()) -> item().
item_kind_module(Module) ->
    #{
        label => Module,
        kind => ?COMPLETION_ITEM_KIND_MODULE,
        insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
    }.

-spec behaviour_modules(list()) -> [atom()].
behaviour_modules(Begin) ->
    Candidates = els_dt_document:find_candidates_with_otp(callback, 'module'),
    Behaviours = [
        els_uri:module(Uri)
     || Uri <- Candidates,
        lists:prefix(Begin, atom_to_list(els_uri:module(Uri))),
        is_behaviour(Uri)
    ],
    Behaviours.

-spec is_behaviour(uri()) -> boolean().
is_behaviour(Uri) ->
    case els_dt_document:lookup(Uri) of
        {ok, [Document]} ->
            [] =/= els_dt_document:pois(Document, [callback]);
        _ ->
            false
    end.

%%==============================================================================
%% Functions, Types, Macros and Records
%%==============================================================================
-spec unexported_definitions(els_dt_document:item(), els_poi:poi_kind()) -> items().
unexported_definitions(Document, any) ->
    unexported_definitions(Document, function) ++
        unexported_definitions(Document, type_definition);
unexported_definitions(Document, POIKind) ->
    AllDefs = definitions(Document, POIKind, arity_only, false),
    ExportedDefs = definitions(Document, POIKind, arity_only, true),
    AllDefs -- ExportedDefs.

-spec definitions(els_dt_document:item(), els_poi:poi_kind()) -> [map()].
definitions(Document, POIKind) ->
    definitions(Document, POIKind, _ItemFormat = args, _ExportedOnly = false).

-spec definitions(els_dt_document:item(), poi_kind_or_any(), item_format()) -> [map()].
definitions(Document, any, ItemFormat) ->
    definitions(Document, function, ItemFormat) ++
        definitions(Document, type_definition, ItemFormat);
definitions(Document, POIKind, ItemFormat) ->
    definitions(Document, POIKind, ItemFormat, _ExportedOnly = false).

-spec definitions(els_dt_document:item(), els_poi:poi_kind(), item_format(), boolean()) ->
    [map()].
definitions(Document, POIKind, ItemFormat, ExportedOnly) ->
    POIs = els_scope:local_and_included_pois(Document, POIKind),
    #{uri := Uri} = Document,
    %% Find exported entries when there is an export_entry kind available
    FAs =
        case export_entry_kind(POIKind) of
            {error, no_export_entry_kind} ->
                [];
            ExportKind ->
                Exports = els_scope:local_and_included_pois(Document, ExportKind),
                [FA || #{id := FA} <- Exports]
        end,
    Items = resolve_definitions(Uri, POIs, FAs, ExportedOnly, ItemFormat),
    lists:usort(Items).

-spec completion_context(els_dt_document:item(), line(), column(), tokens()) ->
    {item_format(), els_poi:poi_kind() | any}.
completion_context(#{text := Text} = Document, Line, Column, Tokens) ->
    ItemFormat =
        case is_in_mfa_list_attr(Document, Line, Column) of
            true ->
                arity_only;
            false ->
                case els_text:get_char(Text, Line, Column + 1) of
                    {ok, $(} ->
                        %% Don't inlude args if next character is a '('
                        no_args;
                    _ ->
                        args
                end
        end,
    POIKind =
        case
            is_in(
                Document,
                Line,
                Column,
                [spec, export_type, type_definition]
            )
        of
            true ->
                type_definition;
            false ->
                case is_in(Document, Line, Column, [export, nifs, function]) of
                    true ->
                        function;
                    false ->
                        poikind_from_tokens(Tokens)
                end
        end,
    {ItemFormat, POIKind}.

-spec is_in_mfa_list_attr(els_dt_document:item(), line(), column()) -> boolean().
is_in_mfa_list_attr(#{text := Text} = Document, Line, Column) ->
    %% Sometimes is_in will be confused because e.g. -export() failed to be parsed.
    %% In such case we can use a heuristic to determine if we are inside
    %% an export.
    is_in(Document, Line, Column, [export, export_type, nifs]) orelse
        is_in_mfa_list_attr_heuristic(Text, Line - 1).

-spec is_in_mfa_list_attr_heuristic(binary(), line()) -> boolean().
is_in_mfa_list_attr_heuristic(Text, Line) ->
    is_in_heuristic(Text, <<"export">>, Line) orelse
        is_in_heuristic(Text, <<"nifs">>, Line).

-spec is_in_heuristic(binary(), binary(), line()) -> boolean().
is_in_heuristic(Text, Attr, Line) ->
    Len = byte_size(Attr),
    case els_text:line(Text, Line) of
        <<"-", Attr:Len/binary, _/binary>> ->
            %% In Attr
            true;
        <<" ", _/binary>> when Line > 1 ->
            %% Indented line, continue to search previous line
            is_in_heuristic(Text, Attr, Line - 1);
        _ ->
            false
    end.

-spec resolve_definitions(
    uri(),
    [els_poi:poi()],
    [{atom(), arity()}],
    boolean(),
    item_format()
) ->
    [map()].
resolve_definitions(Uri, Functions, ExportsFA, ExportedOnly, ItemFormat) ->
    [
        resolve_definition(Uri, POI, ItemFormat)
     || #{id := FA} = POI <- Functions,
        not ExportedOnly orelse lists:member(FA, ExportsFA)
    ].

-spec resolve_definition(uri(), els_poi:poi(), item_format()) -> map().
resolve_definition(Uri, #{kind := 'function', id := {F, A}} = POI, ItemFormat) ->
    Data = #{
        <<"module">> => els_uri:module(Uri),
        <<"function">> => F,
        <<"arity">> => A
    },
    completion_item(POI, Data, ItemFormat, Uri);
resolve_definition(
    Uri,
    #{kind := 'type_definition', id := {T, A}} = POI,
    ItemFormat
) ->
    Data = #{
        <<"module">> => els_uri:module(Uri),
        <<"type">> => T,
        <<"arity">> => A
    },
    completion_item(POI, Data, ItemFormat, Uri);
resolve_definition(Uri, POI, ItemFormat) ->
    completion_item(POI, #{}, ItemFormat, Uri).

-spec exported_definitions(module(), els_poi:poi_kind(), item_format()) -> [map()].
exported_definitions(Module, any, ItemFormat) ->
    exported_definitions(Module, function, ItemFormat) ++
        exported_definitions(Module, type_definition, ItemFormat);
exported_definitions(Module, POIKind, ItemFormat) ->
    case els_utils:find_module(Module) of
        {ok, Uri} ->
            case els_utils:lookup_document(Uri) of
                {ok, Document} ->
                    definitions(Document, POIKind, ItemFormat, true);
                {error, _} ->
                    []
            end;
        {error, _Error} ->
            []
    end.

%%==============================================================================
%% Variables
%%==============================================================================

-spec variables(els_dt_document:item()) -> [map()].
variables(Document) ->
    POIs = els_dt_document:pois(Document, [variable]),
    Vars = [
        #{
            label => atom_to_binary(Name, utf8),
            kind => ?COMPLETION_ITEM_KIND_VARIABLE
        }
     || #{id := Name} <- POIs
    ],
    lists:usort(Vars).

%%==============================================================================
%%  Record Fields
%%==============================================================================

-spec all_record_fields(els_dt_document:item(), binary()) -> [map()].
all_record_fields(Document, Prefix) ->
    POIs = els_scope:local_and_included_pois(Document, [
        record_def_field,
        record_field
    ]),
    Fields = [Id || #{id := {_Record, Id}} <- POIs],
    Unique = lists:usort(Fields),
    filter_by_prefix(Prefix, Unique, fun atom_to_label/1, fun item_kind_field/1).

-spec record_fields(els_dt_document:item(), atom()) -> [map()].
record_fields(Document, RecordName) ->
    case find_record_definition(Document, RecordName) of
        [] ->
            [];
        POIs ->
            [#{data := #{field_list := Fields}} | _] = els_poi:sort(POIs),
            [
                item_kind_field(atom_to_label(Name))
             || Name <- Fields
            ]
    end.

-spec record_fields_with_var(els_dt_document:item(), atom()) -> [map()].
record_fields_with_var(Document, RecordName) ->
    case find_record_definition(Document, RecordName) of
        [] ->
            [];
        POIs ->
            [#{data := #{field_list := Fields}} | _] = els_poi:sort(POIs),
            SnippetSupport = snippet_support(),
            Format =
                case SnippetSupport of
                    true -> ?INSERT_TEXT_FORMAT_SNIPPET;
                    false -> ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                end,
            [
                #{
                    label => atom_to_label(Name),
                    kind => ?COMPLETION_ITEM_KIND_FIELD,
                    insertText => format_record_field_with_var(Name, SnippetSupport),
                    insertTextFormat => Format
                }
             || Name <- Fields
            ]
    end.

-spec format_record_field_with_var(atom(), SnippetSupport :: boolean()) -> binary().
format_record_field_with_var(Name, true) ->
    Label = atom_to_label(Name),
    Var = els_utils:camel_case(Label),
    <<Label/binary, " = ${1:", Var/binary, "}">>;
format_record_field_with_var(Name, false) ->
    atom_to_label(Name).

-spec find_record_definition(els_dt_document:item(), atom()) -> [els_poi:poi()].
find_record_definition(Document, RecordName) ->
    POIs = els_scope:local_and_included_pois(Document, record),
    [X || X = #{id := Name} <- POIs, Name =:= RecordName].

-spec item_kind_field(binary()) -> map().
item_kind_field(Name) ->
    #{
        label => Name,
        kind => ?COMPLETION_ITEM_KIND_FIELD
    }.

%%==============================================================================
%% Keywords
%%==============================================================================
-spec keywords(poi_kind_or_any(), item_format()) -> [map()].
keywords(type_definition, _ItemFormat) ->
    [];
keywords(_POIKind, arity_only) ->
    [];
keywords(_POIKind, _ItemFormat) ->
    Keywords = keywords(),
    [
        keyword_completion_item(K, snippet_support())
     || K <- Keywords
    ].

-spec keywords() -> [atom()].
keywords() ->
    [
        'after',
        'and',
        'andalso',
        'band',
        'begin',
        'bnot',
        'bor',
        'bsl',
        'bsr',
        'bxor',
        'case',
        'catch',
        'cond',
        'div',
        'end',
        'else',
        'fun',
        'if',
        'let',
        'maybe',
        'not',
        'of',
        'or',
        'orelse',
        'receive',
        'rem',
        'try',
        'when',
        'xor'
    ].

-spec keyword_completion_item(_, _) -> _.
keyword_completion_item('case', true) ->
    #{
        label => <<"case">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "case ${1:Exprs} of\n"
                "  ${2:Pattern} ->\n"
                "    ${3:Body}\n"
                "end"
            >>
    };
keyword_completion_item('try', true) ->
    #{
        label => <<"try">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "try ${1:Exprs}\n"
                "catch\n"
                "  ${2:Class}:${3:ExceptionPattern}:${4:Stacktrace} ->\n"
                "    ${5:ExceptionBody}\n"
                "end"
            >>
    };
keyword_completion_item('catch', true) ->
    #{
        label => <<"catch">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "catch\n"
                "  ${1:Class}:${2:ExceptionPattern}:${3:Stacktrace} ->\n"
                "    ${4:ExceptionBody}\n"
                "end"
            >>
    };
keyword_completion_item('begin', true) ->
    #{
        label => <<"begin">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "begin\n"
                "  ${1:Body}\n"
                "end"
            >>
    };
keyword_completion_item('maybe', true) ->
    #{
        label => <<"maybe">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "maybe\n"
                "  ${1:Body}\n"
                "end"
            >>
    };
keyword_completion_item('after', true) ->
    #{
        label => <<"after">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "after\n"
                "  ${1:Duration} ->\n"
                "    ${2:Body}"
            >>
    };
keyword_completion_item('else', true) ->
    #{
        label => <<"else">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "else\n"
                "  ${1:Pattern} ->\n"
                "    ${2:Body}"
            >>
    };
keyword_completion_item('of', true) ->
    #{
        label => <<"of">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "of\n"
                "  ${1:Pattern} ->\n"
                "    ${2:Body}"
            >>
    };
keyword_completion_item('receive', true) ->
    #{
        label => <<"receive">>,
        kind => ?COMPLETION_ITEM_KIND_KEYWORD,
        insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
        insertText =>
            <<
                "receive\n"
                "  ${1:Pattern} ->\n"
                "    ${2:Body}\n"
                "end"
            >>
    };
keyword_completion_item(K, _SnippetSupport) ->
    #{
        label => atom_to_binary(K, utf8),
        kind => ?COMPLETION_ITEM_KIND_KEYWORD
    }.

%%==============================================================================
%% Built-in functions
%%==============================================================================

-spec bifs(poi_kind_or_any(), item_format()) -> [map()].
bifs(type_definition, arity_only) ->
    %% We don't want to include the built-in types when we are in
    %% a -export_types(). context.
    [];
bifs(Kind, ItemFormat) ->
    [completion_item(X, ItemFormat) || X <- bif_pois(Kind)].

-spec bif_pois(poi_kind_or_any()) -> [map()].
bif_pois(any) ->
    bif_pois(function) ++ bif_pois(type_definition);
bif_pois(function) ->
    Range = #{from => {0, 0}, to => {0, 0}},
    Exports = erlang:module_info(exports),
    [
        #{
            kind => function,
            id => X,
            range => Range,
            data => #{args => generate_arguments("Arg", A)}
        }
     || {F, A} = X <- Exports, erl_internal:bif(F, A)
    ];
bif_pois(type_definition) ->
    Types = [
        {'any', 0},
        {'arity', 0},
        {'atom', 0},
        {'binary', 0},
        {'bitstring', 0},
        {'boolean', 0},
        {'byte', 0},
        {'char', 0},
        {'float', 0},
        {'fun', 0},
        {'fun', 1},
        {'function', 0},
        {'identifier', 0},
        {'integer', 0},
        {'iodata', 0},
        {'iolist', 0},
        {'list', 0},
        {'list', 1},
        {'map', 0},
        {'maybe_improper_list', 0},
        {'maybe_improper_list', 2},
        {'mfa', 0},
        {'module', 0},
        {'neg_integer', 0},
        {'nil', 0},
        {'no_return', 0},
        {'node', 0},
        {'nonempty_binary', 0},
        {'nonempty_bitstring', 0},
        {'nonempty_improper_list', 2},
        {'nonempty_list', 1},
        {'non_neg_integer', 0},
        {'none', 0},
        {'nonempty_list', 0},
        {'nonempty_string', 0},
        {'number', 0},
        {'pid', 0},
        {'port', 0},
        {'pos_integer', 0},
        {'reference', 0},
        {'string', 0},
        {'term', 0},
        {'timeout', 0}
    ],
    Range = #{from => {0, 0}, to => {0, 0}},
    [
        #{
            kind => type_definition,
            id => X,
            range => Range,
            data => #{args => generate_arguments("Type", A)}
        }
     || {_, A} = X <- Types
    ];
bif_pois(define) ->
    Macros = [
        {'MODULE', none},
        {'MODULE_STRING', none},
        {'FILE', none},
        {'LINE', none},
        {'MACHINE', none},
        {'FUNCTION_NAME', none},
        {'FUNCTION_ARITY', none},
        {'OTP_RELEASE', none},
        {{'FEATURE_AVAILABLE', 1}, [#{index => 1, name => "Feature"}]},
        {{'FEATURE_ENABLED', 1}, [#{index => 1, name => "Feature"}]}
    ],
    Range = #{from => {0, 0}, to => {0, 0}},
    [
        #{
            kind => define,
            id => Id,
            range => Range,
            data => #{args => Args}
        }
     || {Id, Args} <- Macros
    ].

-spec generate_arguments(string(), integer()) -> els_arg:args().
generate_arguments(Prefix, Arity) ->
    [
        els_arg:new(N, Prefix ++ integer_to_list(N))
     || N <- lists:seq(1, Arity)
    ].

%%==============================================================================
%% Filter by prefix
%%==============================================================================

%% TODO: Implement as select
-spec filter_by_prefix(binary(), [binary()], function(), function()) -> [map()].
filter_by_prefix(Prefix, List, ToBinary, ItemFun) ->
    FilterMapFun = fun(X) ->
        Str = ToBinary(X),
        case string:prefix(Str, Prefix) of
            nomatch -> false;
            _ -> {true, ItemFun(Str)}
        end
    end,
    lists:filtermap(FilterMapFun, List).

%%==============================================================================
%% Helper functions
%%==============================================================================
-spec completion_item(els_poi:poi(), item_format()) -> map().
completion_item(POI, ItemFormat) ->
    completion_item(POI, #{}, ItemFormat, undefined).

-spec completion_item(els_poi:poi(), map(), item_format(), uri() | undefined) -> map().
completion_item(#{kind := Kind, id := {F, A}} = POI, Data, args, Uri) when
    Kind =:= function;
    Kind =:= type_definition
->
    Args = args(POI, Uri),
    Label = io_lib:format("~p/~p", [F, A]),
    SnippetSupport = snippet_support(),
    Format =
        case SnippetSupport of
            true -> ?INSERT_TEXT_FORMAT_SNIPPET;
            false -> ?INSERT_TEXT_FORMAT_PLAIN_TEXT
        end,
    #{
        label => els_utils:to_binary(Label),
        kind => completion_item_kind(Kind),
        insertText => format_function(F, Args, SnippetSupport, Kind),
        insertTextFormat => Format,
        data => Data
    };
completion_item(#{kind := Kind, id := {F, A}}, Data, no_args, _Uri) when
    Kind =:= function;
    Kind =:= type_definition
->
    Label = io_lib:format("~p/~p", [F, A]),
    #{
        label => els_utils:to_binary(Label),
        kind => completion_item_kind(Kind),
        insertText => atom_to_label(F),
        insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
        data => Data
    };
completion_item(#{kind := Kind, id := {F, A}}, Data, arity_only, _Uri) when
    Kind =:= function;
    Kind =:= type_definition
->
    Label = io_lib:format("~p/~p", [F, A]),
    #{
        label => els_utils:to_binary(Label),
        kind => completion_item_kind(Kind),
        insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
        data => Data
    };
completion_item(#{kind := Kind = record, id := Name}, Data, _, _Uri) ->
    #{
        label => atom_to_label(Name),
        kind => completion_item_kind(Kind),
        data => Data
    };
completion_item(#{kind := Kind = define, id := Name, data := Info}, Data, _, _Uri) ->
    #{args := ArgNames} = Info,
    SnippetSupport = snippet_support(),
    Format =
        case SnippetSupport of
            true -> ?INSERT_TEXT_FORMAT_SNIPPET;
            false -> ?INSERT_TEXT_FORMAT_PLAIN_TEXT
        end,
    #{
        label => macro_label(Name),
        kind => completion_item_kind(Kind),
        insertText => format_macro(Name, ArgNames, SnippetSupport),
        insertTextFormat => Format,
        data => Data
    }.

-spec args(els_poi:poi(), uri()) -> els_arg:args().
args(#{kind := type_definition, data := POIData}, _Uri) ->
    maps:get(args, POIData);
args(#{kind := _Kind, data := POIData}, _Uri = undefined) ->
    maps:get(args, POIData);
args(#{kind := function} = POI, Uri) ->
    els_arg:get_args(Uri, POI).

-spec features() -> items().
features() ->
    %% Hardcoded for now. Could use erl_features:all() in the future.
    Features = [maybe_expr],
    [
        #{
            label => atom_to_binary(Feature, utf8),
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            insertText => atom_to_binary(Feature, utf8),
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{}
        }
     || Feature <- Features
    ].

-spec macro_label(atom() | {atom(), non_neg_integer()}) -> binary().
macro_label({Name, Arity}) ->
    els_utils:to_binary(
        io_lib:format(
            "~ts/~p",
            [macro_to_label(Name), Arity]
        )
    );
macro_label(Name) ->
    macro_to_label(Name).

-spec macro_to_label(atom()) -> binary().
macro_to_label(Name) ->
    %% Trick to ensure we can handle macros like ?'FOO BAR'.
    Bin = atom_to_binary(Name, utf8),
    case re:run(Bin, "\s", [{capture, none}]) of
        nomatch ->
            Bin;
        match ->
            atom_to_label(Name)
    end.

-spec format_function(atom(), els_arg:args(), boolean(), els_poi:poi_kind()) -> binary().
format_function(Name, Args, SnippetSupport, Kind) ->
    format_args(atom_to_label(Name), Args, SnippetSupport, Kind).

-spec format_macro(
    atom() | {atom(), non_neg_integer()},
    els_arg:args(),
    boolean()
) -> binary().
format_macro({Name0, _Arity}, Args, SnippetSupport) ->
    Name = macro_to_label(Name0),
    format_args(Name, Args, SnippetSupport, define);
format_macro(Name, none, _SnippetSupport) ->
    macro_to_label(Name).

-spec format_args(
    binary(),
    els_arg:args(),
    boolean(),
    els_poi:poi_kind()
) -> binary().
format_args(Name, Args0, SnippetSupport, Kind) ->
    Args =
        case SnippetSupport of
            false ->
                [];
            true ->
                ArgList = [format_arg(Arg, Kind) || Arg <- Args0],
                ["(", string:join(ArgList, ", "), ")"]
        end,
    els_utils:to_binary([Name | Args]).

-spec format_arg(els_arg:arg(), els_poi:poi_kind()) -> iolist().
format_arg(Arg, Kind) ->
    [
        "${",
        els_arg:index(Arg),
        ":",
        els_arg:name(prefix(Kind), Arg),
        "}"
    ].

-spec prefix(els_poi:poi_kind()) -> string().
prefix(type_definition) ->
    "Type";
prefix(_) ->
    "Arg".

-spec snippet_support() -> boolean().
snippet_support() ->
    case els_config:get(capabilities) of
        #{
            <<"textDocument">> :=
                #{
                    <<"completion">> :=
                        #{
                            <<"completionItem">> :=
                                #{<<"snippetSupport">> := SnippetSupport}
                        }
                }
        } ->
            SnippetSupport;
        _ ->
            false
    end.

-spec is_in(els_dt_document:item(), line(), column(), [els_poi:poi_kind()]) ->
    boolean().
is_in(Document, Line, Column, POIKinds) ->
    POIs = match_all_pos(els_dt_document:pois(Document), {Line, Column}),
    IsKind = fun(#{kind := Kind}) -> lists:member(Kind, POIKinds) end,
    lists:any(IsKind, POIs).

-spec match_all_pos([els_poi:poi()], pos()) -> [els_poi:poi()].
match_all_pos(POIs, Pos) ->
    lists:usort(
        [
            POI
         || #{range := #{from := From, to := To}} = POI <- POIs,
            (From =< Pos) andalso (Pos =< To)
        ] ++
            [
                POI
             || #{
                    data := #{
                        wrapping_range :=
                            #{from := From, to := To}
                    }
                } = POI <- POIs,
                (From =< Pos) andalso (Pos =< To)
            ]
    ).

%% @doc Maps a POI kind to its completion item kind
-spec completion_item_kind(els_poi:poi_kind()) -> completion_item_kind().
completion_item_kind(define) ->
    ?COMPLETION_ITEM_KIND_CONSTANT;
completion_item_kind(record) ->
    ?COMPLETION_ITEM_KIND_STRUCT;
completion_item_kind(type_definition) ->
    ?COMPLETION_ITEM_KIND_TYPE_PARAM;
completion_item_kind(function) ->
    ?COMPLETION_ITEM_KIND_FUNCTION.

%% @doc Maps a POI kind to its export entry POI kind
-spec export_entry_kind(els_poi:poi_kind()) ->
    els_poi:poi_kind() | {error, no_export_entry_kind}.
export_entry_kind(type_definition) -> export_type_entry;
export_entry_kind(function) -> export_entry;
export_entry_kind(_) -> {error, no_export_entry_kind}.

-spec atom_to_label(atom()) -> binary().
atom_to_label(Atom) when is_atom(Atom) ->
    unicode:characters_to_binary(io_lib:write(Atom)).

%%==============================================================================
%% Tests
%%==============================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_record_test() ->
    ?assertEqual(
        {ok, foo},
        parse_record(<<"#foo">>, <<"{}.">>)
    ),
    ?assertEqual(
        {ok, foo},
        parse_record(<<"#foo{x = y">>, <<"}.">>)
    ),
    ?assertEqual(
        {ok, foo},
        parse_record(<<"#foo{x = #bar{}">>, <<"}.">>)
    ),
    ?assertEqual(
        {ok, foo},
        parse_record(<<"#foo{x = #bar{y = #baz{}}">>, <<"}.">>)
    ).

is_exported_heuristic_test_() ->
    Text = <<
        "-module(test).\n"
        "-export([foo/0\n"
        "         bar/0\n"
        "         baz/0\n"
        "        ]).\n"
        "-define(FOO, foo).\n"
    >>,
    [
        ?_assertEqual(false, is_in_mfa_list_attr_heuristic(Text, 0)),
        ?_assertEqual(true, is_in_mfa_list_attr_heuristic(Text, 1)),
        ?_assertEqual(true, is_in_mfa_list_attr_heuristic(Text, 2)),
        ?_assertEqual(true, is_in_mfa_list_attr_heuristic(Text, 3)),
        ?_assertEqual(true, is_in_mfa_list_attr_heuristic(Text, 4)),
        ?_assertEqual(false, is_in_mfa_list_attr_heuristic(Text, 5))
    ].

-endif.
