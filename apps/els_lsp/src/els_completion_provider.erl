-module(els_completion_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    handle_request/1,
    trigger_characters/0
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
        on_complete =>
            fun(Resp) ->
                els_server ! {result, Resp, self()},
                ok
            end
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
    #{trigger := <<"-">>, document := _Document, column := 1}
) ->
    attributes();
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
    [item_kind_file(Path) || Path <- paths_include_lib()];
find_completions(
    <<"-include(">>,
    ?COMPLETION_TRIGGER_KIND_CHARACTER,
    #{trigger := <<"\"">>, document := Document}
) ->
    [item_kind_file(Path) || Path <- paths_include(Document)];
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
            attributes();
        %% Check for "-export(["
        [{'[', _}, {'(', _}, {atom, _, export}, {'-', _}] ->
            unexported_definitions(Document, function);
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
        [{atom, _, _}, {'(', _}, {atom, _, Attribute}, {'-', _}] when
            Attribute =:= behaviour; Attribute =:= behavior
        ->
            [item_kind_module(Module) || Module <- behaviour_modules()];
        %% Check for "-behaviour("
        [{'(', _}, {atom, _, Attribute}, {'-', _}] when
            Attribute =:= behaviour; Attribute =:= behavior
        ->
            [item_kind_module(Module) || Module <- behaviour_modules()];
        %% Check for "[...] fun atom"
        [{atom, _, _}, {'fun', _} | _] ->
            bifs(function, ItemFormat = arity_only) ++
                definitions(Document, function, ItemFormat = arity_only);
        %% Check for "| atom"
        [{atom, _, Name}, {'|', _} | _] = Tokens ->
            {ItemFormat, _POIKind} =
                completion_context(Document, Line, Column, Tokens),
            complete_type_definition(Document, Name, ItemFormat);
        %% Check for ":: atom"
        [{atom, _, Name}, {'::', _} | _] = Tokens ->
            {ItemFormat, _POIKind} =
                completion_context(Document, Line, Column, Tokens),
            complete_type_definition(Document, Name, ItemFormat);
        %% Check for "[...] atom"
        [{atom, _, Name} | _] = Tokens ->
            NameBinary = atom_to_binary(Name, utf8),
            {ItemFormat, POIKind} = completion_context(Document, Line, Column, Tokens),
            case ItemFormat of
                arity_only ->
                    %% Only complete unexported definitions when in export
                    unexported_definitions(Document, POIKind);
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
    POIs = els_dt_document:pois(Document, [function]),
    %% Look for record start between current pos and end of last function
    Prefix =
        case els_scope:pois_before(POIs, #{from => Pos, to => Pos}) of
            [#{range := #{to := {Line, _}}} | _] ->
                {_, Prefix1} = els_text:split_at_line(Prefix0, Line),
                Prefix1;
            _ ->
                %% No function before, consider all the text
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
-spec attributes() -> items().
attributes() ->
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
        snippet(attribute_opaque),
        snippet(attribute_record),
        snippet(attribute_type),
        snippet(attribute_vsn)
    ].

%%=============================================================================
%% Include paths
%%=============================================================================
-spec paths_include(els_dt_document:item()) -> [binary()].
paths_include(#{uri := Uri}) ->
    case match_in_path(els_uri:path(Uri), els_config:get(apps_paths)) of
        [] ->
            [];
        [Path | _] ->
            AppPath = filename:join(lists:droplast(filename:split(Path))),
            {ok, Headers} = els_dt_document_index:find_by_kind(header),
            lists:flatmap(
                fun(#{uri := HeaderUri}) ->
                    case string:prefix(els_uri:path(HeaderUri), AppPath) of
                        nomatch ->
                            [];
                        IncludePath ->
                            [relative_include_path(IncludePath)]
                    end
                end,
                Headers
            )
    end.

-spec paths_include_lib() -> [binary()].
paths_include_lib() ->
    Paths =
        els_config:get(otp_paths) ++
            els_config:get(deps_paths) ++
            els_config:get(apps_paths) ++
            els_config:get(include_paths),
    {ok, Headers} = els_dt_document_index:find_by_kind(header),
    lists:flatmap(
        fun(#{uri := Uri}) ->
            HeaderPath = els_uri:path(Uri),
            case match_in_path(HeaderPath, Paths) of
                [] ->
                    [];
                [Path | _] ->
                    <<"/", PathSuffix/binary>> = string:prefix(HeaderPath, Path),
                    PathBin = unicode:characters_to_binary(Path),
                    case lists:reverse(filename:split(PathBin)) of
                        [<<"include">>, App | _] ->
                            [
                                filename:join([
                                    strip_app_version(App),
                                    <<"include">>,
                                    PathSuffix
                                ])
                            ];
                        _ ->
                            []
                    end
            end
        end,
        Headers
    ).

-spec match_in_path(binary(), [binary()]) -> [binary()].
match_in_path(DocumentPath, Paths) ->
    [P || P <- Paths, string:prefix(DocumentPath, P) =/= nomatch].

-spec relative_include_path(binary()) -> binary().
relative_include_path(Path) ->
    case filename:split(Path) of
        [_App, <<"include">> | Rest] -> filename:join(Rest);
        [_App, <<"src">> | Rest] -> filename:join(Rest);
        [_App, SubDir | Rest] -> filename:join([<<"..">>, SubDir | Rest])
    end.

-spec strip_app_version(binary()) -> binary().
strip_app_version(App0) ->
    %% Transform "foo-1.0" into "foo"
    case string:lexemes(App0, "-") of
        [] ->
            App0;
        [_] ->
            App0;
        Lexemes ->
            Vsn = lists:last(Lexemes),
            case re:run(Vsn, "^[0-9.]+$", [global, {capture, none}]) of
                match -> list_to_binary(lists:join("-", lists:droplast(Lexemes)));
                nomatch -> App0
            end
    end.

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

-spec behaviour_modules() -> [atom()].
behaviour_modules() ->
    {ok, Modules} = els_dt_document_index:find_by_kind(module),
    OtpBehaviours = [
        gen_event,
        gen_server,
        gen_statem,
        supervisor
    ],
    Behaviours = [Id || #{id := Id, uri := Uri} <- Modules, is_behaviour(Uri)],
    OtpBehaviours ++ Behaviours.

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
        case is_in(Document, Line, Column, [export, export_type]) of
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
                case is_in(Document, Line, Column, [export, function]) of
                    true ->
                        function;
                    false ->
                        poikind_from_tokens(Tokens)
                end
        end,
    {ItemFormat, POIKind}.

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
    completion_item(POI, Data, ItemFormat);
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
    completion_item(POI, Data, ItemFormat);
resolve_definition(_Uri, POI, ItemFormat) ->
    completion_item(POI, ItemFormat).

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
    Keywords = [
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
        'fun',
        'if',
        'let',
        'not',
        'of',
        'or',
        'orelse',
        'receive',
        'rem',
        'try',
        'when',
        'xor'
    ],
    [
        #{
            label => atom_to_binary(K, utf8),
            kind => ?COMPLETION_ITEM_KIND_KEYWORD
        }
     || K <- Keywords
    ].

%%==============================================================================
%% Built-in functions
%%==============================================================================

-spec bifs(poi_kind_or_any(), item_format()) -> [map()].
bifs(any, ItemFormat) ->
    bifs(function, ItemFormat) ++
        bifs(type_definition, ItemFormat);
bifs(function, ItemFormat) ->
    Range = #{from => {0, 0}, to => {0, 0}},
    Exports = erlang:module_info(exports),
    BIFs = [
        #{
            kind => function,
            id => X,
            range => Range,
            data => #{args => generate_arguments("Arg", A)}
        }
     || {F, A} = X <- Exports, erl_internal:bif(F, A)
    ],
    [completion_item(X, ItemFormat) || X <- BIFs];
bifs(type_definition, arity_only) ->
    %% We don't want to include the built-in types when we are in
    %% a -export_types(). context.
    [];
bifs(type_definition, ItemFormat) ->
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
    POIs = [
        #{
            kind => type_definition,
            id => X,
            range => Range,
            data => #{args => generate_arguments("Type", A)}
        }
     || {_, A} = X <- Types
    ],
    [completion_item(X, ItemFormat) || X <- POIs];
bifs(define, ItemFormat) ->
    Macros = [
        {'MODULE', none},
        {'MODULE_STRING', none},
        {'FILE', none},
        {'LINE', none},
        {'MACHINE', none},
        {'FUNCTION_NAME', none},
        {'FUNCTION_ARITY', none},
        {'OTP_RELEASE', none},
        {{'FEATURE_AVAILABLE', 1}, [{1, "Feature"}]},
        {{'FEATURE_ENABLED', 1}, [{1, "Feature"}]}
    ],
    Range = #{from => {0, 0}, to => {0, 0}},
    POIs = [
        #{
            kind => define,
            id => Id,
            range => Range,
            data => #{args => Args}
        }
     || {Id, Args} <- Macros
    ],
    [completion_item(X, ItemFormat) || X <- POIs].

-spec generate_arguments(string(), integer()) -> [{integer(), string()}].
generate_arguments(Prefix, Arity) ->
    [{N, Prefix ++ integer_to_list(N)} || N <- lists:seq(1, Arity)].

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
    completion_item(POI, #{}, ItemFormat).

-spec completion_item(els_poi:poi(), map(), item_format()) -> map().
completion_item(#{kind := Kind, id := {F, A}, data := POIData}, Data, args) when
    Kind =:= function;
    Kind =:= type_definition
->
    ArgsNames = maps:get(args, POIData),
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
        insertText => format_function(F, ArgsNames, SnippetSupport),
        insertTextFormat => Format,
        data => Data
    };
completion_item(#{kind := Kind, id := {F, A}}, Data, no_args) when
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
completion_item(#{kind := Kind, id := {F, A}}, Data, arity_only) when
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
completion_item(#{kind := Kind = record, id := Name}, Data, _) ->
    #{
        label => atom_to_label(Name),
        kind => completion_item_kind(Kind),
        data => Data
    };
completion_item(#{kind := Kind = define, id := Name, data := Info}, Data, _) ->
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
    els_utils:to_binary(io_lib:format("~ts/~p", [Name, Arity]));
macro_label(Name) ->
    atom_to_binary(Name, utf8).

-spec format_function(atom(), [{integer(), string()}], boolean()) -> binary().
format_function(Name, Args, SnippetSupport) ->
    format_args(atom_to_label(Name), Args, SnippetSupport).

-spec format_macro(
    atom() | {atom(), non_neg_integer()},
    [{integer(), string()}],
    boolean()
) -> binary().
format_macro({Name0, _Arity}, Args, SnippetSupport) ->
    Name = atom_to_binary(Name0, utf8),
    format_args(Name, Args, SnippetSupport);
format_macro(Name, none, _SnippetSupport) ->
    atom_to_binary(Name, utf8).

-spec format_args(binary(), [{integer(), string()}], boolean()) -> binary().
format_args(Name, Args0, SnippetSupport) ->
    Args =
        case SnippetSupport of
            false ->
                [];
            true ->
                ArgList = [["${", integer_to_list(N), ":", A, "}"] || {N, A} <- Args0],
                ["(", string:join(ArgList, ", "), ")"]
        end,
    els_utils:to_binary([Name | Args]).

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

strip_app_version_test() ->
    ?assertEqual(<<"foo">>, strip_app_version(<<"foo">>)),
    ?assertEqual(<<"foo">>, strip_app_version(<<"foo-1.2.3">>)),
    ?assertEqual(<<"">>, strip_app_version(<<"">>)),
    ?assertEqual(<<"foo-bar">>, strip_app_version(<<"foo-bar">>)),
    ?assertEqual(<<"foo-bar">>, strip_app_version(<<"foo-bar-1.2.3">>)),
    ?assertEqual(<<"foo-bar-baz">>, strip_app_version(<<"foo-bar-baz">>)),
    ?assertEqual(<<"foo-bar-baz">>, strip_app_version(<<"foo-bar-baz-1.2.3">>)).

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

-endif.
