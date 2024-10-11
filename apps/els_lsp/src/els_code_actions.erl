-module(els_code_actions).
-export([
    extract_function/2,
    create_function/4,
    export_function/4,
    fix_module_name/4,
    ignore_variable/4,
    remove_macro/4,
    remove_unused/4,
    suggest_variable/4,
    fix_atom_typo/4,
    undefined_callback/4,
    define_macro/4,
    define_record/4,
    add_include_lib_macro/4,
    add_include_lib_record/4,
    suggest_macro/4,
    suggest_record/4,
    suggest_record_field/4,
    suggest_function/4,
    suggest_module/4,
    bump_variables/2
]).

-include("els_lsp.hrl").
-spec create_function(uri(), range(), binary(), [binary()]) -> [map()].
create_function(Uri, Range0, _Data, [UndefinedFun]) ->
    {ok, #{text := Text} = Document} = els_utils:lookup_document(Uri),
    Range = els_range:to_poi_range(Range0),
    Indent = guess_indentation(string:lexemes(Text, "\n")),
    IndentStr = lists:duplicate(Indent, 32),
    FunPOIs = els_dt_document:pois(Document, [function]),
    %% Figure out which function the error was found in, as we want to
    %% create the function right after the current function.
    %% (Where the wrapping_range ends)
    case
        [
            R
         || #{data := #{wrapping_range := R}} <- FunPOIs,
            els_range:in(Range, R)
        ]
    of
        [#{to := {Line, _}} | _] ->
            [Name, ArityBin] = string:split(UndefinedFun, "/"),
            Arity = binary_to_integer(ArityBin),
            Args = format_args(Document, Arity, Range),
            SpecAndFun = io_lib:format(
                "~s(~s) ->\n~sok.\n\n",
                [Name, Args, IndentStr]
            ),
            [
                make_edit_action(
                    Uri,
                    <<"Create function ", UndefinedFun/binary>>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    iolist_to_binary(SpecAndFun),
                    els_protocol:range(#{
                        from => {Line + 1, 1},
                        to => {Line + 1, 1}
                    })
                )
            ];
        _ ->
            []
    end.

-spec export_function(uri(), range(), binary(), [binary()]) -> [map()].
export_function(Uri, _Range, _Data, [UnusedFun]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    case els_poi:sort(els_dt_document:pois(Document, [module, export])) of
        [] ->
            [];
        POIs ->
            #{range := #{to := {Line, _Col}}} = lists:last(POIs),
            Pos = {Line + 1, 1},
            [
                make_edit_action(
                    Uri,
                    <<"Export ", UnusedFun/binary>>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    <<"-export([", UnusedFun/binary, "]).\n">>,
                    els_protocol:range(#{from => Pos, to => Pos})
                )
            ]
    end.

-spec ignore_variable(uri(), range(), binary(), [binary()]) -> [map()].
ignore_variable(Uri, Range, _Data, [UnusedVariable]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
    case ensure_range(els_range:to_poi_range(Range), UnusedVariable, POIs) of
        {ok, VarRange} ->
            [
                make_edit_action(
                    Uri,
                    <<"Add '_' to '", UnusedVariable/binary, "'">>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    <<"_", UnusedVariable/binary>>,
                    els_protocol:range(VarRange)
                )
            ];
        error ->
            []
    end.

-spec add_include_lib_macro(uri(), range(), binary(), [binary()]) -> [map()].
add_include_lib_macro(Uri, Range, _Data, [Macro0]) ->
    {Name, Id} =
        case string:split(Macro0, "/") of
            [MacroBin] ->
                Name0 = binary_to_atom(MacroBin, utf8),
                {Name0, Name0};
            [MacroBin, ArityBin] ->
                Name0 = binary_to_atom(MacroBin, utf8),
                Arity = binary_to_integer(ArityBin),
                {Name0, {Name0, Arity}}
        end,
    add_include_file(Uri, Range, 'define', Name, Id).

-spec define_macro(uri(), range(), binary(), [binary()]) -> [map()].
define_macro(Uri, Range, _Data, [Macro0]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    NewText =
        case string:split(Macro0, "/") of
            [MacroBin] ->
                <<"-define(", MacroBin/binary, ", undefined).\n">>;
            [MacroBin, ArityBin] ->
                Arity = binary_to_integer(ArityBin),
                Args = string:join(lists:duplicate(Arity, "_"), ", "),
                list_to_binary(
                    ["-define(", MacroBin, "(", Args, "), undefined).\n"]
                )
        end,
    #{from := Pos} = els_range:to_poi_range(Range),
    BeforeRange = #{from => {1, 1}, to => Pos},
    POIs = els_dt_document:pois_in_range(
        Document,
        [module, include, include_lib, define],
        BeforeRange
    ),
    case POIs of
        [] ->
            [];
        _ ->
            #{range := #{to := {Line, _}}} = lists:last(els_poi:sort(POIs)),
            [
                make_edit_action(
                    Uri,
                    <<"Define ", Macro0/binary>>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    NewText,
                    els_protocol:range(#{
                        to => {Line + 1, 1},
                        from => {Line + 1, 1}
                    })
                )
            ]
    end.

-spec define_record(uri(), range(), binary(), [binary()]) -> [map()].
define_record(Uri, Range, _Data, [Record]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    NewText = <<"-record(", Record/binary, ", {}).\n">>,
    #{from := Pos} = els_range:to_poi_range(Range),
    BeforeRange = #{from => {1, 1}, to => Pos},
    POIs = els_dt_document:pois_in_range(
        Document,
        [module, include, include_lib, record],
        BeforeRange
    ),
    case POIs of
        [] ->
            [];
        _ ->
            Line = end_line(lists:last(els_poi:sort(POIs))),
            [
                make_edit_action(
                    Uri,
                    <<"Define record ", Record/binary>>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    NewText,
                    els_protocol:range(#{
                        to => {Line + 1, 1},
                        from => {Line + 1, 1}
                    })
                )
            ]
    end.

-spec end_line(els_poi:poi()) -> non_neg_integer().
end_line(#{data := #{value_range := #{to := {Line, _}}}}) ->
    Line;
end_line(#{range := #{to := {Line, _}}}) ->
    Line.

-spec add_include_lib_record(uri(), range(), _, [binary()]) -> [map()].
add_include_lib_record(Uri, Range, _Data, [Record]) ->
    Name = binary_to_atom(Record, utf8),
    add_include_file(Uri, Range, 'record', Name, Name).

-spec add_include_file(uri(), range(), els_poi:poi_kind(), atom(), els_poi:poi_id()) -> [map()].
add_include_file(Uri, Range, Kind, Name, Id) ->
    %% TODO: Add support for -include() also
    CandidateUris =
        els_dt_document:find_candidates_with_otp(Name, 'header'),
    Uris = [
        CandidateUri
     || CandidateUri <- CandidateUris,
        contains_poi(Kind, CandidateUri, Id)
    ],
    Paths = els_include_paths:include_libs(Uris),
    {ok, Document} = els_utils:lookup_document(Uri),
    #{from := Pos} = els_range:to_poi_range(Range),
    BeforeRange = #{from => {1, 1}, to => Pos},
    case
        els_dt_document:pois_in_range(
            Document,
            [module, include, include_lib],
            BeforeRange
        )
    of
        [] ->
            [];
        POIs ->
            #{range := #{to := {Line, _}}} = lists:last(els_poi:sort(POIs)),
            [
                make_edit_action(
                    Uri,
                    <<"Add -include_lib(\"", Path/binary, "\")">>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    <<"-include_lib(\"", Path/binary, "\").\n">>,
                    els_protocol:range(#{to => {Line + 1, 1}, from => {Line + 1, 1}})
                )
             || Path <- Paths
            ]
    end.

-spec contains_poi(els_poi:poi_kind(), uri(), atom()) -> boolean().
contains_poi(Kind, Uri, Macro) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:pois(Document, [Kind]),
    lists:any(fun(#{id := Id}) -> Id =:= Macro end, POIs).

-spec suggest_variable(uri(), range(), binary(), [binary()]) -> [map()].
suggest_variable(Uri, Range, _Data, [Var]) ->
    %% Supply a quickfix to replace an unbound variable with the most similar
    %% variable name in scope.
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
    case ensure_range(els_range:to_poi_range(Range), Var, POIs) of
        {ok, VarRange} ->
            ScopeRange = els_scope:variable_scope_range(VarRange, Document),
            VarsInScope = [
                atom_to_binary(Id, utf8)
             || #{range := R, id := Id} <- POIs,
                els_range:in(R, ScopeRange),
                els_range:compare(R, VarRange)
            ],
            VariableDistances =
                [{els_utils:jaro_distance(V, Var), V} || V <- VarsInScope, V =/= Var],
            [
                make_edit_action(
                    Uri,
                    <<"Did you mean '", V/binary, "'?">>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    V,
                    els_protocol:range(VarRange)
                )
             || {Distance, V} <- lists:reverse(lists:usort(VariableDistances)),
                Distance > 0.8
            ];
        error ->
            []
    end.

-spec suggest_macro(uri(), range(), binary(), [binary()]) -> [map()].
suggest_macro(Uri, Range, _Data, [Macro]) ->
    %% Supply a quickfix to replace an unbound variable with the most similar
    %% variable name in scope.
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs =
        els_scope:local_and_included_pois(Document, [define]) ++
            els_completion_provider:bif_pois(define),
    {Name, MacrosInScope} =
        case string:split(Macro, "/") of
            [Name0] ->
                {Name0, [atom_to_binary(Id) || #{id := Id} <- POIs, is_atom(Id)]};
            [Name0, ArityBin] ->
                Arity = binary_to_integer(ArityBin),
                {Name0, [
                    atom_to_binary(Id)
                 || #{id := {Id, A}} <- POIs,
                    is_atom(Id),
                    A =:= Arity
                ]}
        end,
    Distances =
        [{els_utils:jaro_distance(M, Name), M} || M <- MacrosInScope, M =/= Macro],
    [
        make_edit_action(
            Uri,
            <<"Did you mean '", M/binary, "'?">>,
            ?CODE_ACTION_KIND_QUICKFIX,
            <<"?", M/binary>>,
            Range
        )
     || {Distance, M} <- lists:reverse(lists:usort(Distances)),
        Distance > 0.8
    ].

-spec suggest_record(uri(), range(), binary(), [binary()]) -> [map()].
suggest_record(Uri, Range, _Data, [Record]) ->
    %% Supply a quickfix to replace an unrecognized record with the most similar
    %% record in scope.
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_scope:local_and_included_pois(Document, [record]),
    RecordsInScope = [atom_to_binary(Id) || #{id := Id} <- POIs, is_atom(Id)],
    Distances =
        [{els_utils:jaro_distance(Rec, Record), Rec} || Rec <- RecordsInScope, Rec =/= Record],
    [
        make_edit_action(
            Uri,
            <<"Did you mean #", Rec/binary, "{}?">>,
            ?CODE_ACTION_KIND_QUICKFIX,
            <<"#", Rec/binary>>,
            Range
        )
     || {Distance, Rec} <- lists:reverse(lists:usort(Distances)),
        Distance > 0.8
    ].

-spec suggest_record_field(uri(), range(), binary(), [binary()]) -> [map()].
suggest_record_field(Uri, Range, _Data, [Field, Record]) ->
    %% Supply a quickfix to replace an unrecognized record field with the most
    %% similar record field in Record.
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_scope:local_and_included_pois(Document, [record]),
    RecordId = binary_to_atom(Record, utf8),
    Fields = [
        atom_to_binary(F)
     || #{id := Id, data := #{field_list := Fs}} <- POIs,
        F <- Fs,
        Id =:= RecordId
    ],
    Distances =
        [{els_utils:jaro_distance(F, Field), F} || F <- Fields, F =/= Field],
    [
        make_edit_action(
            Uri,
            <<"Did you mean #", Record/binary, ".", F/binary, "?">>,
            ?CODE_ACTION_KIND_QUICKFIX,
            <<F/binary>>,
            Range
        )
     || {Distance, F} <- lists:reverse(lists:usort(Distances)),
        Distance > 0.8
    ].

-spec suggest_function(uri(), range(), binary(), [binary()]) -> [map()].
suggest_function(Uri, Range, _Data, [FunBin]) ->
    [ModNameBin, _ArityBin] = string:split(FunBin, <<"/">>),
    {{ok, Document}, NameBin} =
        case string:split(ModNameBin, <<":">>) of
            [ModBin, NameBin0] ->
                Mod = binary_to_atom(ModBin, utf8),
                {ok, ModUri} = els_utils:find_module(Mod),
                {els_utils:lookup_document(ModUri), NameBin0};
            [NameBin0] ->
                {els_utils:lookup_document(Uri), NameBin0}
        end,
    POIs = els_dt_document:pois(Document, [function]),
    Funs = [atom_to_binary(F) || #{id := {F, _A}} <- POIs],
    Distances =
        [{els_utils:jaro_distance(F, NameBin), F} || F <- Funs, F =/= NameBin],
    [
        make_edit_action(
            Uri,
            <<"Did you mean ", F/binary, "?">>,
            ?CODE_ACTION_KIND_QUICKFIX,
            F,
            Range
        )
     || {Distance, F} <- lists:reverse(lists:usort(Distances)),
        Distance > 0.8
    ].

-spec suggest_module(uri(), range(), binary(), [binary()]) -> [map()].
suggest_module(Uri, Range, _Data, [NameBin]) ->
    {ok, Items} = els_dt_document_index:find_by_kind(module),
    Mods = [atom_to_binary(M) || #{id := M} <- Items],
    Distances =
        [{els_utils:jaro_distance(M, NameBin), M} || M <- Mods, M =/= NameBin],
    [
        make_edit_action(
            Uri,
            <<"Did you mean ", M/binary, "?">>,
            ?CODE_ACTION_KIND_QUICKFIX,
            M,
            Range
        )
     || {Distance, M} <- lists:reverse(lists:usort(Distances)),
        Distance > 0.8
    ].

-spec fix_module_name(uri(), range(), binary(), [binary()]) -> [map()].
fix_module_name(Uri, Range0, _Data, [ModName, FileName]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_poi:sort(els_dt_document:pois(Document, [module])),
    case ensure_range(els_range:to_poi_range(Range0), ModName, POIs) of
        {ok, Range} ->
            [
                make_edit_action(
                    Uri,
                    <<"Change to -module(", FileName/binary, ").">>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    FileName,
                    els_protocol:range(Range)
                )
            ];
        error ->
            []
    end.

-spec remove_macro(uri(), range(), binary(), [binary()]) -> [map()].
remove_macro(Uri, Range, _Data, [Macro]) ->
    %% Supply a quickfix to remove the unused Macro
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_poi:sort(els_dt_document:pois(Document, [define])),
    case ensure_range(els_range:to_poi_range(Range), Macro, POIs) of
        {ok, MacroRange} ->
            LineRange = els_range:line(MacroRange),
            [
                make_edit_action(
                    Uri,
                    <<"Remove unused macro ", Macro/binary, ".">>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    <<"">>,
                    els_protocol:range(LineRange)
                )
            ];
        error ->
            []
    end.

-spec remove_unused(uri(), range(), binary(), [binary()]) -> [map()].
remove_unused(_Uri, _Range0, <<>>, [_Import]) ->
    [];
remove_unused(Uri, _Range0, Data, [Import]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    case els_range:inclusion_range(Data, Document) of
        {ok, UnusedRange} ->
            LineRange = els_range:line(UnusedRange),
            [
                make_edit_action(
                    Uri,
                    <<"Remove unused -include_lib(", Import/binary, ").">>,
                    ?CODE_ACTION_KIND_QUICKFIX,
                    <<>>,
                    els_protocol:range(LineRange)
                )
            ];
        error ->
            []
    end.

-spec fix_atom_typo(uri(), range(), binary(), [binary()]) -> [map()].
fix_atom_typo(Uri, Range, _Data, [Atom]) ->
    [
        make_edit_action(
            Uri,
            <<"Fix typo: ", Atom/binary>>,
            ?CODE_ACTION_KIND_QUICKFIX,
            Atom,
            Range
        )
    ].

-spec extract_function(uri(), range()) -> [map()].
extract_function(Uri, Range) ->
    {ok, [Document]} = els_dt_document:lookup(Uri),
    #{from := From = {Line, Column}, to := To} = els_range:to_poi_range(Range),
    %% We only want to extract if selection is large enough
    %% and cursor is inside a function
    case
        large_enough_range(From, To) andalso
            not contains_function_clause(Document, Line) andalso
            els_dt_document:wrapping_functions(Document, Line, Column) /= []
    of
        true ->
            [
                #{
                    title => <<"Extract function">>,
                    kind => <<"refactor.extract">>,
                    command => make_extract_function_command(Range, Uri)
                }
            ];
        false ->
            []
    end.

-spec bump_variables(uri(), range()) -> [map()].
bump_variables(Uri, Range) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    #{from := {Line, Column}} = els_range:to_poi_range(Range),
    POIs = els_dt_document:get_element_at_pos(Document, Line, Column),
    case [POI || #{kind := variable} = POI <- POIs] of
        [] ->
            [];
        [#{id := Id, range := PoiRange} = _POI | _] ->
            Name = atom_to_binary(Id),
            case ends_with_digit(Name) of
                false ->
                    [];
                true ->
                    VarRange = els_protocol:range(PoiRange),
                    [
                        #{
                            title => <<"Bump variables: ", Name/binary>>,
                            kind => ?CODE_ACTION_KIND_QUICKFIX,
                            command => make_bump_variables_command(VarRange, Uri, Name)
                        }
                    ]
            end
    end.

-spec ends_with_digit(binary()) -> boolean().
ends_with_digit(Bin) ->
    N = binary:last(Bin),
    $0 =< N andalso N =< $9.

-spec make_extract_function_command(range(), uri()) -> map().
make_extract_function_command(Range, Uri) ->
    els_command:make_command(
        <<"Extract function">>,
        <<"refactor.extract">>,
        [#{uri => Uri, range => Range}]
    ).

-spec make_bump_variables_command(range(), uri(), binary()) -> map().
make_bump_variables_command(Range, Uri, Name) ->
    els_command:make_command(
        <<"Bump variables">>,
        <<"bump-variables">>,
        [#{uri => Uri, range => Range, name => Name}]
    ).

-spec contains_function_clause(
    els_dt_document:item(),
    non_neg_integer()
) -> boolean().
contains_function_clause(Document, Line) ->
    POIs = els_dt_document:get_element_at_pos(Document, Line, 1),
    lists:any(
        fun
            (#{kind := 'function_clause'}) ->
                true;
            (_) ->
                false
        end,
        POIs
    ).

-spec large_enough_range(pos(), pos()) -> boolean().
large_enough_range({Line, FromC}, {Line, ToC}) when (ToC - FromC) < 2 ->
    false;
large_enough_range(_From, _To) ->
    true.

-spec undefined_callback(uri(), range(), binary(), [binary()]) -> [map()].
undefined_callback(Uri, _Range, _Data, [_Function, Behaviour]) ->
    Title = <<"Add missing callbacks for: ", Behaviour/binary>>,
    [
        #{
            title => Title,
            kind => ?CODE_ACTION_KIND_QUICKFIX,
            command =>
                els_command:make_command(
                    Title,
                    <<"add-behaviour-callbacks">>,
                    [
                        #{
                            uri => Uri,
                            behaviour => Behaviour
                        }
                    ]
                )
        }
    ].

-spec ensure_range(els_poi:poi_range(), binary(), [els_poi:poi()]) ->
    {ok, els_poi:poi_range()} | error.
ensure_range(#{from := {Line, _}}, SubjectId, POIs) ->
    SubjectAtom = binary_to_atom(SubjectId, utf8),
    Ranges = [
        R
     || #{range := R, id := Id} <- POIs,
        els_range:in(R, #{from => {Line, 1}, to => {Line + 1, 1}}),
        Id =:= SubjectAtom
    ],
    case Ranges of
        [] ->
            error;
        [Range | _] ->
            {ok, Range}
    end.

-spec make_edit_action(uri(), binary(), binary(), binary(), range()) ->
    map().
make_edit_action(Uri, Title, Kind, Text, Range) ->
    #{
        title => Title,
        kind => Kind,
        edit => edit(Uri, Text, Range)
    }.

-spec edit(uri(), binary(), range()) -> workspace_edit().
edit(Uri, Text, Range) ->
    #{changes => #{Uri => [#{newText => Text, range => Range}]}}.

-spec format_args(
    els_dt_document:item(),
    non_neg_integer(),
    els_poi:poi_range()
) -> string().
format_args(Document, Arity, Range) ->
    %% Find the matching function application and extract
    %% argument names from it.
    AppPOIs = els_dt_document:pois(Document, [application]),
    Matches = [
        POI
     || #{range := R} = POI <- AppPOIs,
        els_range:in(R, Range)
    ],
    case Matches of
        [#{data := #{args := Args0}} | _] ->
            string:join([els_arg:name(A) || A <- Args0], ", ");
        [] ->
            string:join(lists:duplicate(Arity, "_"), ", ")
    end.

-spec guess_indentation([binary()]) -> pos_integer().
guess_indentation([]) ->
    2;
guess_indentation([A, B | Rest]) ->
    ACount = count_leading_spaces(A, 0),
    BCount = count_leading_spaces(B, 0),
    case {ACount, BCount} of
        {0, N} when N > 0 ->
            N;
        {_, _} ->
            guess_indentation([B | Rest])
    end.

-spec count_leading_spaces(binary(), non_neg_integer()) -> non_neg_integer().
count_leading_spaces(<<>>, _Acc) ->
    0;
count_leading_spaces(<<" ", Rest/binary>>, Acc) ->
    count_leading_spaces(Rest, 1 + Acc);
count_leading_spaces(<<_:8, _/binary>>, Acc) ->
    Acc.
