-module(els_code_actions).
-export([
    create_function/4,
    export_function/4,
    fix_module_name/4,
    ignore_variable/4,
    remove_macro/4,
    remove_unused/4,
    suggest_variable/4,
    fix_atom_typo/4,
    undefined_callback/4
]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

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
            string:join([A || {_N, A} <- Args0], ", ");
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
