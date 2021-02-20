%% Copyright (c) Facebook, Inc. and its affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc AST conversion between erlfmt and syntax_tools.

-module(els_erlfmt_ast).

-export([erlfmt_to_st/1, st_to_erlfmt/1]).

% dialyzer hates erlfmt_parse:abstract_node()
-type erlfmt() :: term().
-type syntax_tools() :: erl_syntax:syntaxTree().

-spec erlfmt_to_st(Node :: erlfmt()) -> syntax_tools().
%% @doc Convert from erlfmt ASTs to Syntax Tools ASTs.

%% Note: the erl_syntax library still refers to the 2nd element as "pos"
%% even though it has morphed into a generic annotation in erl_parse trees
%% (represented as property lists, or as maps in the erlfmt
%% representation). Actual erl_syntax nodes have an additional annotation
%% field, separate from the position info, but this is not being used here.
%% Hence, the erl_syntax:set_pos() function is used for all annotations.

erlfmt_to_st(Node) ->
    case Node of
        %% ---------------------------------------------------------------------
        %% The following cases can be easily rewritten without losing information

        %% The special `match` node is encoded as a regular binary operator
        {op, Pos, '=', Left, Right} ->
            erlfmt_to_st_1({match, Pos, Left, Right});
        %% The special `catch` node is encoded as a regular unary operator
        {op, Pos, 'catch', Expr} ->
            erlfmt_to_st_1({'catch', Pos, Expr});
        %% Type annotations are represented as :: operators
        {op, Pos, '::', Left, Right} ->
            erlfmt_to_st_1({ann_type, Pos, [Left, Right]});
        %% ---------------------------------------------------------------------
        %% Whenever simply rewriting the node to the corresponding standard
        %% erl_parse form would discard information (such as annotations on
        %% atoms which are stored naked in the erl_parse format), we must
        %% construct the node using the erl_syntax API, which supports
        %% preserving annotations on such sub-fields.

        %% raw strings only occur as forms, for when parsing the form failed
        {raw_string, Pos, Text} ->
            erl_syntax:set_pos(erl_syntax:text("\n>>>>\n" ++ Text ++ "\n<<<<\n"), Pos);
        %% A new node `{macro_call, Anno, Name, Args}` is introduced, where
        %% `Name` is either an `atom` or a `var` node and `Args` is a list of
        %% expressions, types, or special `op` nodes with `'when'` operator.
        {macro_call, Pos, Name, Args} ->
            Args1 =
                case Args of
                    none -> none;
                    _ -> [erlfmt_to_st(A) || A <- Args]
                end,
            erl_syntax:set_pos(erl_syntax:macro(erlfmt_to_st(Name), Args1), Pos);
        %% The value of an attribute node is always a list of abstract term
        %% formats instead of concrete terms. The name is always represented
        %% as a full `atom` node.
        {attribute, Pos, {atom, _, record} = Tag, [Name, Tuple]} ->
            %% The record name is represented as node instead of a raw atom
            %% and typed record fields are represented as '::' ops
            {tuple, TPos, Fields} = Tuple,
            Fields1 = [
                case F of
                    {op, FPos, '::', B, T} ->
                        B1 = erlfmt_to_st(B),
                        T1 = erlfmt_to_st(T),
                        erl_syntax:set_pos(
                            erl_syntax:typed_record_field(B1, T1),
                            FPos
                        );
                    _ ->
                        erlfmt_to_st(F)
                end
                || F <- Fields
            ],
            Tuple1 = erl_syntax:set_pos(erl_syntax:tuple(Fields1), TPos),
            erl_syntax:set_pos(
                erl_syntax:attribute(
                    erlfmt_to_st(Tag),
                    [
                        erlfmt_to_st(Name),
                        Tuple1
                    ]
                ),
                Pos
            );
        %% Representation for types is in general the same as for
        %% corresponding values. The `type` node is not used at all. This
        %% means new binary operators `|`, `::`, and `..` inside types.
        %% {attribute, Pos, {atom, _, Tag}, [Def]} when Tag =:= type; Tag =:= opaque ->
        %%     {op, _OPos, '::', {call, _CPos, {atom, _, Name}, Args}, Type} = Def,
        %%     %% must ensure plain erl_parse format for these subterms for now
        %%     %% because of the incomplete handling of -type attrs in erl_syntax
        %%     Type1 = erl_syntax:revert(erlfmt_to_st(Type)),
        %%     Args1 = [erl_syntax:revert(erlfmt_to_st(A)) || A <- Args],
        %%     Attr = {attribute, Pos, type, {Name, Type1, Args1}},
        %%     erlfmt_to_st_1(Attr);
        {attribute, Pos, {atom, _, Tag}, [_Def]} when
            Tag =:= type; Tag =:= opaque; Tag =:= spec; Tag =:= callback
        ->
            %% TODO: FIXME - passing types/specs through as raw text for now
            Text = unicode:characters_to_list(erlfmt:format_nodes([Node], 100)),
            erlfmt_to_st({raw_string, Pos, "[[reparse]]" ++ Text});
        {attribute, Pos, {atom, _, define} = Tag, [Name, empty]} ->
            %% the erlfmt parser allows defines with empty bodies (with the
            %% closing parens following after the comma); we must turn the
            %% atom 'empty' into a proper node here
            Body = erl_syntax:set_pos(erl_syntax:text(""), dummy_anno()),
            erl_syntax:set_pos(
                erl_syntax:attribute(
                    erlfmt_to_st(Tag),
                    [
                        erlfmt_to_st(Name),
                        Body
                    ]
                ),
                Pos
            );
        {attribute, Pos, Name, no_parens} ->
            %% a directive without parentheses, like -endif.
            erl_syntax:set_pos(erl_syntax:attribute(erlfmt_to_st(Name)), Pos);
        %% Attributes are not processed to convert the `fun/arity` syntax into
        %% tuples, they are left as the `op` nodes with the `/` operator.
        %% Additionally, the `import` and `export` attributes are not
        %% processed to convert the `cons` node chains into lists and contain
        %% `list` nodes.
        {attribute, Pos, Name, Args} ->
            %% general attributes -Name(Arg1, ... ArgN)
            %% (Name is not a naked atom, so Node is not erl_parse compatible)
            Args1 = [fold_arity_qualifiers(erlfmt_to_st(A)) || A <- Args],
            erl_syntax:set_pos(erl_syntax:attribute(erlfmt_to_st(Name), Args1), Pos);
        %% The `function` node has a different AST representation: `{function,
        %% Anno, Clauses}`, where `Clauses` is a list of `clause` nodes or
        %% `macro_call` nodes. Additionally it is less strict - it does not
        %% enforce all clauses have the same name and arity.
        {function, Pos, Clauses} ->
            case get_function_name(Clauses) of
                none ->
                    %% treat clauses as a list of regular nodes
                    %% (presumably macro calls) and use an empty text node
                    %% as the function name
                    Clauses1 = [erlfmt_to_st(C) || C <- Clauses],
                    Name = erl_syntax:set_pos(erl_syntax:text(""), dummy_anno()),
                    erl_syntax:set_pos(
                        erl_syntax:function(
                            Name,
                            Clauses1
                        ),
                        Pos
                    );
                Name ->
                    Clauses1 = [erlfmt_clause_to_st(C) || C <- Clauses],
                    erl_syntax:set_pos(
                        erl_syntax:function(
                            erlfmt_to_st(Name),
                            Clauses1
                        ),
                        Pos
                    )
            end;
        {'try', Pos, {body, _, _} = Body, Clauses, Handlers, After} ->
            %% TODO: preserving annotations on bodies and clause groups
            Body1 = [erlfmt_to_st(Body)],
            Clauses1 = case Clauses of
                         {clauses, _, CList} ->
                           [erlfmt_clause_to_st(C) || C <- CList];
                         none ->
                           []
                       end,
            Handlers1 = case Handlers of
                          {clauses, _, HList} ->
                            [erlfmt_clause_to_st(C) || C <- HList];
                          none ->
                            []
                        end,
            After1 = [erlfmt_to_st(E) || E <- After],
            erl_syntax:set_pos(
                erl_syntax:try_expr(
                    Body1,
                    Clauses1,
                    Handlers1,
                    After1
                ),
                Pos
            );
        {clause, Pos, {call, CPos, Name, Args}, Guard, Body} ->
            %% free standing named clause - make a magic tuple to
            %% hold both the name and the clause with the args
            AAnno = dummy_anno(),
            Clause = {clause, Pos, {args, CPos, Args}, Guard, Body},
            erlfmt_to_st_1({tuple, CPos, [{atom, AAnno, '*named_clause*'}, Name, Clause]});
        {clause, _, _, _, _} = Clause ->
            %% clauses of case/if/receive/try
            erlfmt_clause_to_st(Clause);
        %% Lists are represented as a `list` node instead of a chain of `cons`
        %% and `nil` nodes, similar to the `tuple` node. The last elemenent of
        %% the list can be a `cons` node representing explicit consing syntax.
        {list, Pos, Elements} ->
            %% a "cons" node here means 'H | T' in isolation
            %% and can only exist at the end of a list body
            {Es, Tail} =
                case lists:reverse(Elements) of
                    [{cons, _CPos, H, T} | Rest] ->
                        {lists:reverse([H | Rest]), erlfmt_to_st(T)};
                    _ ->
                        {Elements, none}
                end,
            Es1 = [erlfmt_to_st(E) || E <- Es],
            erl_syntax:set_pos(erl_syntax:list(Es1, Tail), Pos);
        %% The record name is always represented as node instead of a raw atom
        {record, Pos, Name, Fields} ->
            % a new record instance
            Fields1 = [erlfmt_to_st(F) || F <- Fields],
            erl_syntax:set_pos(
                erl_syntax:record_expr(
                    erlfmt_to_st(Name),
                    Fields1
                ),
                Pos
            );
        {record, Pos, Expr, Name, Fields} ->
            % updating a record
            Fields1 = [erlfmt_to_st(F) || F <- Fields],
            erl_syntax:set_pos(
                erl_syntax:record_expr(
                    erlfmt_to_st(Expr),
                    erlfmt_to_st(Name),
                    Fields1
                ),
                Pos
            );
        {record_field, Pos, Name} ->
            %% a record field without value, just the field name
            erl_syntax:set_pos(erl_syntax:record_field(erlfmt_to_st(Name)), Pos);
        {record_field, Pos, Name, Value} ->
            %% a record field "name = val"
            erl_syntax:set_pos(
                erl_syntax:record_field(
                    erlfmt_to_st(Name),
                    erlfmt_to_st(Value)
                ),
                Pos
            );
        {record_field, Pos, Expr, Record, Field} ->
            %% a record field access expression "expr#record.field"
            erl_syntax:set_pos(
                erl_syntax:record_access(
                    erlfmt_to_st(Expr),
                    erlfmt_to_st(Record),
                    erlfmt_to_st(Field)
                ),
                Pos
            );
        {record_index, Pos, Record, Field} ->
            %% a record field index "#record.field"
            erl_syntax:set_pos(
                erl_syntax:record_index_expr(
                    erlfmt_to_st(Record),
                    erlfmt_to_st(Field)
                ),
                Pos
            );
        %% The `fun` node has a different AST representation:
        %% `{'fun', Anno, Value}`, where `Value` is one of:
        %% * `{function, Anno, Name, Arity}`, where `Name` and `Arity` are an
        %%   `atom` and `integer` node respectively or `var` or `macro_call`
        %%   nodes.
        %% * `{function, Anno, Module, Name, Arity}`, where `Module`, `Name`,
        %%   and `Arity` are `atom`, `atom`, and `integer` nodes respectively
        %%   or a `var` or `macro_call` node.
        %% * `{clauses, Anno, Clauses}`, where `Clauses` is a list of `clause`
        %%   nodes. Additionally it is less strict - the clauses aren't
        %%   checked for the same name or arity.
        %% * `type` for the anonymous function type `fun()`.
        %% * `{type, Anno, Args, Res}` for the anonymous function type
        %%   `fun((...Args) -> Res)` where `Args` is a `args` node.
        %% * The `named_fun` node is not used - instead, clauses have a call
        %%   head, just as for plain functions.
        {'fun', Pos, {clauses, _CPos, Clauses}} ->
            %% TODO: can we preserve CPos in any useful way?
            [{clause, _, Head, _, _} | _] = Clauses,
            Clauses1 = [erlfmt_clause_to_st(C) || C <- Clauses],
            case Head of
                {call, _, Name, _} ->
                    %% if the head has function call shape, it's a named fun
                    erl_syntax:set_pos(
                        erl_syntax:named_fun_expr(
                            erlfmt_to_st(Name),
                            Clauses1
                        ),
                        Pos
                    );
                _ ->
                    erl_syntax:set_pos(erl_syntax:fun_expr(Clauses1), Pos)
            end;
        {'fun', Pos, {function, FPos, Name, Arity}} ->
            FName = erl_syntax:set_pos(
                erl_syntax:arity_qualifier(
                    erlfmt_to_st(Name),
                    erlfmt_to_st(Arity)
                ),
                FPos
            ),
            erl_syntax:set_pos(erl_syntax:implicit_fun(FName), Pos);
        {'fun', Pos, {function, FPos, Module, Name, Arity}} ->
            %% note that the inner arity qualifier gets no annotation
            FName = erl_syntax:set_pos(
                erl_syntax:module_qualifier(
                    erlfmt_to_st(Module),
                    erl_syntax:arity_qualifier(
                        erlfmt_to_st(Name),
                        erlfmt_to_st(Arity)
                    )
                ),
                FPos
            ),
            erl_syntax:set_pos(erl_syntax:implicit_fun(FName), Pos);
        %% Bit type definitions inside binaries are represented as full nodes
        %% instead of raw atoms and integers. The unit notation `unit:Int` is
        %% represented with a `{remote, Anno, {atom, Anno, unit}, Int}` node.
        {bin_element, Pos, Expr, Size, Types} when Types =/= default ->
            Types1 = lists:map(
                fun
                    ({remote, QPos, {atom, _, _} = A, {integer, _, _} = I}) ->
                        erl_syntax:set_pos(
                            erl_syntax:size_qualifier(
                                erlfmt_to_st(A),
                                erlfmt_to_st(I)
                            ),
                            QPos
                        );
                    (T) ->
                        erlfmt_to_st(T)
                end,
                Types
            ),
            Size1 =
                case Size of
                    default -> none;
                    _ -> erlfmt_to_st(Size)
                end,
            erl_syntax:set_pos(
                erl_syntax:binary_field(
                    erlfmt_to_st(Expr),
                    Size1,
                    Types1
                ),
                Pos
            );
        %% ---------------------------------------------------------------------
        %% The remaining cases have been added by erlfmt and need special handling
        %% (many are represented as magically-tagged tuples for now)

        %% A new operator node `{op, Anno, 'when', Expr, Guard}` is
        %% introduced, which can occur as a body of a macro. It represents
        %% "free-standing" `Expr when Guard` expressions as used, for
        %% example, in the `assertMatch` macro.
        {op, Pos, 'when', Expr, Guard} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*when*'}, Expr, Guard]});
        %% A new node `{exprs, Anno, Exprs}` represents a
        %% "free-standing" comma separated sequence of expressions
        {exprs, Pos, Exprs} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*exprs*'} | Exprs]});
        %% A new node `{body, Anno, Exprs}` represents a comma separated
        %% sequence of expressions as in 'try ... of/catch'
        {body, Pos, Exprs} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*body*'} | Exprs]});
        %% The erlfmt parser also accepts general guards (comma and
        %% semicolon separated sequences of guard expressions) as the body
        %% of a macro
        {guard_or, Pos, Exprs} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*guard_or*'} | Exprs]});
        {guard_and, Pos, Exprs} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*guard_and*'} | Exprs]});
        %% Record name fragments "#name" may also occur as the body of a macro
        {record_name, Pos, Name} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*record_name*'}, Name]});
        %% A new node `{concat, Anno, Concatables}`, where `Concatables` is a
        %% list of `string`, `var`, and `macro_call` nodes. This is used to
        %% represent implicit string concatenation, for example `"foo" "bar"`.
        {concat, Pos, Subtrees} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*concat*'} | Subtrees]});
        %% A new node `{macro_string, Anno, Name}` is introduced, where `Name`
        %% is either an `atom` or a `var` node. It represents `??Name`.
        {macro_string, Pos, Name} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*stringify*'}, Name]});
        %% erlfmt preserves '...' tokens as nodes (which erl_parse doesn't)
        {'...', Pos} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*...*'}]});
        %% sometimes erlfmt leaves comments as separate nodes
        %% instead of attaching them to another node
        {comment, Pos, Lines} ->
            erl_syntax:set_pos(erl_syntax:comment(Lines), Pos);
        %% erlfmt has a separate entry for shebang nodes; we use raw strings
        {shebang, Pos, Text} ->
            erlfmt_to_st({raw_string, Pos, Text});
        %% args nodes may (in macros) occur free floating
        {args, Pos, Args} ->
            AAnno = dummy_anno(),
            erlfmt_to_st_1({tuple, Pos, [{atom, AAnno, '*args*'} | Args]});
        %% TODO:
        %% New `{spec_clause, Anno, Head, Body, Guards}` node for clauses
        %% inside `spec` and `callback` attributes, similar to the `clause`
        %% node above. It reflects the fact that in specs guards come after
        %% body. The `Head` element is always an `args` node.

        _ ->
            %% all remaining cases can be handled by the default erl_syntax
            %% subtree traversal
            erlfmt_to_st_1(Node)
    end.

%% assuming erl_parse format, compatible with erl_syntax
%% TODO: should convert erlfmt anno to erl_syntax pos+annotation
erlfmt_to_st_1(Node) ->
    case erl_syntax:subtrees(Node) of
        [] ->
            % leaf node
            Node;
        Groups0 ->
            %% recurse and replace the subtrees
            Groups1 = erlfmt_subtrees_to_st(Groups0),
            erl_syntax:update_tree(Node, Groups1)
    end.

erlfmt_subtrees_to_st(Groups) ->
    [
        [
            erlfmt_to_st(Subtree)
            || Subtree <- Group
        ]
        || Group <- Groups
    ].

get_function_name([{clause, _, {call, _, Name, _}, _, _} | _]) ->
    %% take the name node of the first clause with a call shape
    %% TODO: this loses info if not all clauses have the same name
    Name;
get_function_name([_ | Cs]) ->
    get_function_name(Cs);
get_function_name([]) ->
    none.

%% The `clause` node has a different AST representation:
%% `{clause, Anno, Head, Guards, Body}`, where the `Guards` element is either
%% an atom `empty` or a `guard_or` node, and `Head` element is one of:
%%  * regular `call` node for functions and named funs;
%%  * atom `empty` for `if` expressions;
%%  * `{args, Anno, Args}` node for an list of expressions wrapped in parentheses;
%%  * `{catch, Anno, Args}` node for clauses in `catch` clauses, where
%%    2 to 3 arguments represent the various `:` separated syntaxes;
%%  * other expression for `case`, `receive`, "of" part of `try` expression
%%    and simple `catch` clauses without `:`.

%% TODO: can we preserve CPos/APos annotations here somehow?
erlfmt_clause_to_st({clause, Pos, empty, Guard, Body}) ->
    erlfmt_clause_to_st(Pos, [], Guard, Body);
erlfmt_clause_to_st({clause, Pos, {call, _CPos, _, Args}, Guard, Body}) ->
    Patterns = [erlfmt_to_st(A) || A <- Args],
    erlfmt_clause_to_st(Pos, Patterns, Guard, Body);
erlfmt_clause_to_st({clause, Pos, {args, _APos, Args}, Guard, Body}) ->
    Patterns = [erlfmt_to_st(A) || A <- Args],
    erlfmt_clause_to_st(Pos, Patterns, Guard, Body);
erlfmt_clause_to_st({clause, Pos, {'catch', APos, Args}, Guard, Body}) ->
    Pattern =
        case [erlfmt_to_st(A) || A <- Args] of
            [Class, Term] ->
                erl_syntax:set_pos(erl_syntax:class_qualifier(Class, Term), APos);
            [Class, Term, Trace] ->
                erl_syntax:set_pos(erl_syntax:class_qualifier(Class, Term, Trace), APos)
        end,
    erlfmt_clause_to_st(Pos, [Pattern], Guard, Body);
erlfmt_clause_to_st({clause, Pos, Expr, Guard, Body}) ->
    erlfmt_clause_to_st(Pos, [erlfmt_to_st(Expr)], Guard, Body);
erlfmt_clause_to_st(Other) ->
    %% might be a macro call
    erlfmt_to_st(Other).

erlfmt_clause_to_st(Pos, Patterns, Guard, Body) ->
    Groups = [
        Patterns,
        [erlfmt_guard_to_st(Guard)],
        [erlfmt_to_st(B) || B <- Body]
    ],
    erl_syntax:set_pos(erl_syntax:make_tree(clause, Groups), Pos).

%% New `{guard_or, Anno, GuardAndList}` and `{guard_and, Anno, Exprs}` nodes
%% are introduced to support annotating guard sequences, instead of a plain
%% nested list of lists structure.

erlfmt_guard_to_st(empty) ->
    none;
erlfmt_guard_to_st({guard_or, Pos, List}) ->
    erl_syntax:set_pos(
        erl_syntax:disjunction([
            erlfmt_guard_to_st(E)
            || E <- List
        ]),
        Pos
    );
erlfmt_guard_to_st({guard_and, Pos, List}) ->
    erl_syntax:set_pos(
        erl_syntax:conjunction([
            erlfmt_guard_to_st(E)
            || E <- List
        ]),
        Pos
    );
erlfmt_guard_to_st(Other) ->
    erlfmt_to_st(Other).

fold_arity_qualifiers(Tree) ->
    erl_syntax_lib:map(fun fold_arity_qualifier/1, Tree).

fold_arity_qualifier(Node) ->
    case erl_syntax:type(Node) of
        infix_expr ->
            Op = erl_syntax:infix_expr_operator(Node),
            case erl_syntax:type(Op) of
                operator ->
                    case erl_syntax:atom_value(Op) of
                        '/' ->
                            N = erl_syntax:infix_expr_left(Node),
                            A = erl_syntax:infix_expr_right(Node),
                            case
                                erl_syntax:type(N) =:= atom andalso
                                    erl_syntax:type(A) =:= integer
                            of
                                true ->
                                    Q = erl_syntax:arity_qualifier(N, A),
                                    erl_syntax:copy_attrs(Op, Q);
                                false ->
                                    Node
                            end;
                        _ ->
                            Node
                    end;
                _ ->
                    Node
            end;
        _ ->
            Node
    end.

-spec st_to_erlfmt(Node :: syntax_tools()) -> erlfmt().
%% @doc Convert from Syntax Tools ASTs to erlfmt ASTs.

%% this becomes much like erl_syntax:revert/1, though erlfmt is much closer to
%% the representation already used by erl_syntax, so we don't need to do so
%% heavy rewritings, for example for record fields
%% (note that erl_syntax:revert/1 is idempotent and does not do any
%% additional work if the node is no longer an erl_syntax abstract tree,
%% so erlfmt tuples can safely also be passed to revert)

st_to_erlfmt(Node) ->
    %% the input node might be either plain old Abstract Format tuples or
    %% an erl_syntax abstract syntax tree; in either case we must decompose
    %% and recurse to ensure the whole tree is converted to erlfmt tuples
    Pos = erlfmt_anno(erl_syntax:get_pos(Node)),
    case erl_syntax:type(Node) of
        text ->
            case erl_syntax:text_string(Node) of
                "\n>>>>\n" ++ Text0 ->
                    %% note that erlfmt only accepts raw_string as a form
                    "\n<<<<\n" ++ RText = lists:reverse(Text0),
                    case lists:reverse(RText) of
                        "[[reparse]]" ++ Rest ->
                            case erlfmt:read_nodes_string("nofile", Rest) of
                                {ok, [Form], []} ->
                                    Form;
                                _ ->
                                    {raw_string, Pos, Rest}
                            end;
                        "#!" ++ _ = Rest ->
                            {shebang, Pos, Rest};
                        Rest ->
                            {raw_string, Pos, Rest}
                    end;
                "" ->
                    %% assume this is an empty define body or similar
                    empty
            end;
        attribute ->
            Args0 = erl_syntax:attribute_arguments(Node),
            Name = st_to_erlfmt(erl_syntax:attribute_name(Node)),
            case {Args0, Name} of
                {none, _} ->
                    {attribute, Pos, Name, no_parens};
                {_, {atom, _, T}} when T =:= type; T =:= opaque; T =:= spec; T =:= callback ->
                    %% types and specs need special handling, particularly
                    %% since erl_syntax currently treats them as wild attrs
                    st_typedecl_to_erlfmt(Name, Args0, Pos);
                _ ->
                    Args = [st_to_erlfmt(A) || A <- Args0],
                    {attribute, Pos, Name, Args}
            end;
        function ->
            Name = erl_syntax:function_name(Node),
            Clauses = erl_syntax:function_clauses(Node),
            {function, Pos, [
                st_clause_to_erlfmt(C, {name, Name})
                || C <- Clauses
            ]};
        clause ->
            st_clause_to_erlfmt(Node, 'fun');
        nil ->
            {list, Pos, []};
        list ->
            Prefix = [st_to_erlfmt(E) || E <- erl_syntax:list_prefix(Node)],
            Elements =
                case erl_syntax:list_suffix(Node) of
                    none ->
                        Prefix;
                    Suffix ->
                        [Last | RestRev] = lists:reverse(Prefix),
                        Rest = lists:reverse(RestRev),
                        %% have to figure out start and end of cons node
                        LPos = get_anno(Last),
                        SPos = erl_syntax:get_pos(Suffix),
                        CPos = until(SPos, 0, LPos),
                        Rest ++ [{cons, CPos, Last, st_to_erlfmt(Suffix)}]
                end,
            {list, Pos, Elements};
        tuple ->
            %% check for magic tuple tags
            case erl_syntax:tuple_elements(Node) of
                [A | Rest] ->
                    case erl_syntax:type(A) of
                        atom ->
                            case erl_syntax:atom_value(A) of
                                '*concat*' ->
                                    concat_to_erlfmt(Node);
                                '*stringify*' ->
                                    [Name] = Rest,
                                    {macro_string, Pos, st_to_erlfmt(Name)};
                                '*when*' ->
                                    [Expr, Guard] = Rest,
                                    {op, Pos, 'when', st_to_erlfmt(Expr), st_to_erlfmt(Guard)};
                                '*guard_or*' ->
                                    Exprs = [st_to_erlfmt(E) || E <- Rest],
                                    {guard_or, Pos, Exprs};
                                '*guard_and*' ->
                                    Exprs = [st_to_erlfmt(E) || E <- Rest],
                                    {guard_and, Pos, Exprs};
                                '*record_name*' ->
                                    [Name] = Rest,
                                    {record_name, Pos, st_to_erlfmt(Name)};
                                '*named_clause*' ->
                                    [Name, Clause] = Rest,
                                    Name1 = st_to_erlfmt(Name),
                                    {clause, CPos, {args, APos, Args}, Guard, Body} = st_to_erlfmt(
                                        Clause
                                    ),
                                    {clause, CPos, {call, APos, Name1, Args}, Guard, Body};
                                '*...*' when Rest =:= [] ->
                                    {'...', Pos};
                                '*exprs*' ->
                                    Exprs = [st_to_erlfmt(E) || E <- Rest],
                                    {exprs, Pos, Exprs};
                                '*body*' ->
                                    Exprs = [st_to_erlfmt(E) || E <- Rest],
                                    {body, Pos, Exprs};
                                '*args*' ->
                                    Exprs = [st_to_erlfmt(E) || E <- Rest],
                                    {args, Pos, Exprs};
                                _ ->
                                    st_to_erlfmt_1(Node)
                            end;
                        _ ->
                            st_to_erlfmt_1(Node)
                    end;
                _ ->
                    st_to_erlfmt_1(Node)
            end;
        match_expr ->
            [[Left], [Right]] = st_subtrees_to_erlfmt(Node),
            {op, Pos, '=', Left, Right};
        'catch_expr' ->
            [[Expr]] = st_subtrees_to_erlfmt(Node),
            {op, Pos, 'catch', Expr};
        record_field ->
            case st_subtrees_to_erlfmt(Node) of
                [[Name]] ->
                    {record_field, Pos, Name};
                [[Name], [Value]] ->
                    {record_field, Pos, Name, Value}
            end;
        record_expr ->
            case st_subtrees_to_erlfmt(Node) of
                [[Type], Fields] ->
                    {record, Pos, Type, Fields};
                [[Expr], [Type], Fields] ->
                    {record, Pos, Expr, Type, Fields}
            end;
        record_access ->
            [[Expr], [Type], [Field]] = st_subtrees_to_erlfmt(Node),
            {record_field, Pos, Expr, Type, Field};
        record_index_expr ->
            [[Type], [Field]] = st_subtrees_to_erlfmt(Node),
            {record_index, Pos, Type, Field};
        arity_qualifier ->
            %% this handles arity qualifiers in attributes like exports
            [[{atom, FPos, F}], [{integer, APos, A}]] = st_subtrees_to_erlfmt(Node),
            {op, Pos, '/', {atom, erlfmt_anno(FPos), F}, {integer, erlfmt_anno(APos), A}};
        fun_expr ->
            Clauses = erl_syntax:fun_expr_clauses(Node),
            CAnno = dummy_anno(),
            {'fun', Pos,
                {clauses, CAnno, [
                    st_clause_to_erlfmt(C, 'fun')
                    || C <- Clauses
                ]}};
        named_fun_expr ->
            Clauses = erl_syntax:named_fun_expr_clauses(Node),
            Name = erl_syntax:named_fun_expr_name(Node),
            CAnno = dummy_anno(),
            {'fun', Pos,
                {clauses, CAnno, [
                    st_clause_to_erlfmt(C, {name, Name})
                    || C <- Clauses
                ]}};
        implicit_fun ->
            Name = erl_syntax:implicit_fun_name(Node),
            FAnno = erl_syntax:get_pos(Node),
            case erl_syntax:type(Name) of
                arity_qualifier ->
                    F = st_to_erlfmt(erl_syntax:arity_qualifier_body(Name)),
                    A = st_to_erlfmt(erl_syntax:arity_qualifier_argument(Name)),
                    {'fun', Pos, {function, FAnno, F, A}};
                module_qualifier ->
                    M = st_to_erlfmt(erl_syntax:module_qualifier_argument(Name)),
                    Name1 = erl_syntax:module_qualifier_body(Name),
                    %% TODO: can we preserve AAnno somehow, if it is nonempty?
                    _AAnno = erl_syntax:get_pos(Name1),
                    F = st_to_erlfmt(erl_syntax:arity_qualifier_body(Name1)),
                    A = st_to_erlfmt(erl_syntax:arity_qualifier_argument(Name1)),
                    {'fun', Pos, {function, FAnno, M, F, A}}
            end;
        binary_field ->
            Types0 = lists:map(
                fun(E) ->
                    case erl_syntax:type(E) of
                        size_qualifier ->
                            A = erl_syntax:size_qualifier_body(E),
                            I = erl_syntax:size_qualifier_argument(E),
                            RAnno = erl_syntax:get_pos(E),
                            {remote, RAnno, st_to_erlfmt(A), st_to_erlfmt(I)};
                        _ ->
                            st_to_erlfmt(E)
                    end
                end,
                erl_syntax:binary_field_types(Node)
            ),
            Types =
                case Types0 of
                    [] -> default;
                    _ -> Types0
                end,
            Body = erl_syntax:binary_field_body(Node),
            case erl_syntax:type(Body) of
                size_qualifier ->
                    Expr = st_to_erlfmt(erl_syntax:size_qualifier_body(Body)),
                    Size = st_to_erlfmt(erl_syntax:size_qualifier_argument(Body)),
                    {bin_element, Pos, Expr, Size, Types};
                _ ->
                    {bin_element, Pos, st_to_erlfmt(Body), default, Types}
            end;
        macro ->
            case st_subtrees_to_erlfmt(Node) of
                [[Name]] ->
                    {macro_call, Pos, Name, none};
                [[Name], Args] ->
                    {macro_call, Pos, Name, Args}
            end;
        type_application ->
            [[Name], Args] = st_subtrees_to_erlfmt(Node),
            case Name of
                {remote, NPos, M, N} ->
                    exit(remote_type),
                    {remote, NPos, [M, N, Args]};
                _ ->
                    exit({local_type, Name}),
                    {call, Pos, Name, Args}
            end;
        user_type_application ->
            exit({user_type, Node}),
            [[Name], Args] = st_subtrees_to_erlfmt(Node),
            {call, Pos, Name, Args};
        annotated_type ->
            [[Name], [Type]] = st_subtrees_to_erlfmt(Node),
            {op, Pos, '::', Name, Type};
        typed_record_field ->
            [[Body], [Type]] = st_subtrees_to_erlfmt(Node),
            {op, Pos, '::', Body, Type};
        %% for expressions with clauses we cannot first revert the clauses
        %% separately, because the revert of the expression looks at the
        %% clauses and crashes if they are on erlfmt format
        if_expr ->
            {'if', Pos, [
                st_clause_to_erlfmt(C, 'if')
                || C <- erl_syntax:if_expr_clauses(Node)
            ]};
        case_expr ->
            {'case', Pos, st_to_erlfmt(erl_syntax:case_expr_argument(Node)), [
                st_clause_to_erlfmt(C, 'case')
                || C <- erl_syntax:case_expr_clauses(Node)
            ]};
        receive_expr ->
            Clauses = [
                st_clause_to_erlfmt(C, 'case')
                || C <- erl_syntax:receive_expr_clauses(Node)
            ],
            case erl_syntax:receive_expr_timeout(Node) of
                none ->
                    {'receive', Pos, Clauses};
                Timeout ->
                    TBody = [
                        st_to_erlfmt(E)
                        || E <- erl_syntax:receive_expr_action(Node)
                    ],
                    {'receive', Pos, Clauses, st_to_erlfmt(Timeout), TBody}
            end;
        try_expr ->
            [Body0] = erl_syntax:try_expr_body(Node),
            Body = st_to_erlfmt(Body0),
            Clauses = [
                st_clause_to_erlfmt(C, 'case')
                || C <- erl_syntax:try_expr_clauses(Node)
            ],
            Handlers = [
                st_clause_to_erlfmt(C, 'try')
                || C <- erl_syntax:try_expr_handlers(Node)
            ],
            After = [
                st_to_erlfmt(E)
                || E <- erl_syntax:try_expr_after(Node)
            ],
            CAnno = dummy_anno(),
            {'try', Pos, Body, {clauses, CAnno, Clauses}, {clauses, CAnno, Handlers}, After};
        comment ->
            {comment, Pos, erl_syntax:comment_text(Node)};
        _ ->
            st_to_erlfmt_1(Node)
    end.

st_to_erlfmt_1(Node) ->
    %% TODO: should we convert full erl_syntax pos+annotation to erlfmt anno?
    Anno = erlfmt_anno(erl_syntax:get_pos(Node)),
    Anno1 =
        case maps:is_key(text, Anno) of
            true ->
                Anno;
            false ->
                case literal_text(Node) of
                    undefined -> Anno;
                    Text -> Anno#{text => Text}
                end
        end,
    Node1 =
        case st_subtrees_to_erlfmt(Node) of
            [] ->
                %% leaf nodes don't need to replace the subgroups first
                erl_syntax:revert(Node);
            Groups ->
                erl_syntax:revert(erl_syntax:update_tree(Node, Groups))
        end,
    %% certain node types are not revertible out of context; these must be
    %% left as they are and will be handled when reverting the parent node
    case erl_syntax:is_tree(Node1) of
        true ->
            %% use erl_syntax API to put back the adjusted annotation
            erl_syntax:set_pos(Node1, Anno1),
            Node1;
        false ->
            set_anno(Node1, Anno1)
    end.

st_subtrees_to_erlfmt(Node) ->
    case erl_syntax:subtrees(Node) of
        [] ->
            [];
        List ->
            [
                [
                    st_to_erlfmt(Subtree)
                    || Subtree <- Group
                ]
                || Group <- List
            ]
    end.

dummy_anno() ->
    erlfmt_anno(0).

dummy_anno(Text) when is_atom(Text) ->
    (erlfmt_anno(0))#{text => atom_to_list(Text)}.

%% assumes that if the annotation is a map, it came from erlfmt_scan
erlfmt_anno(Map) when is_map(Map) ->
    Map;
erlfmt_anno(Line) ->
    %% have to add end_location as well even if it's incorrect
    #{
        location => {Line, 1},
        % TODO: improve this
        end_location => {Line, 1}
    }.

concat_to_erlfmt(Node) ->
    Es = [st_to_erlfmt(E) || E <- tl(erl_syntax:tuple_elements(Node))],
    {concat, erl_syntax:get_pos(Node), Es}.

st_clause_to_erlfmt(Clause, Kind) ->
    %% check for when a clause is not a clause
    case erl_syntax:type(Clause) of
        clause ->
            st_clause_to_erlfmt_1(Clause, Kind);
        _ ->
            %% presumably the "clause" is a macro call
            %% (this discards the name part for a function clause)
            st_to_erlfmt(Clause)
    end.

st_clause_to_erlfmt_1(Clause, Kind) ->
    Pos = erl_syntax:get_pos(Clause),
    Head =
        case Kind of
            'if' ->
                empty;
            'case' ->
                [Pat] = [
                    st_to_erlfmt(P)
                    || P <- erl_syntax:clause_patterns(Clause)
                ],
                Pat;
            'try' ->
                [Pat] = erl_syntax:clause_patterns(Clause),
                case erl_syntax:type(Pat) of
                    class_qualifier ->
                        A = erl_syntax:class_qualifier_argument(Pat),
                        B = erl_syntax:class_qualifier_body(Pat),
                        T = erl_syntax:class_qualifier_stacktrace(Pat),
                        Ps =
                            case erl_syntax:type(T) of
                                underscore -> [A, B];
                                _ -> [A, B, T]
                            end,
                        %% have to figure out start and end pos for catch node
                        CPos = erl_syntax:get_pos(Clause),
                        PPos = until(right_pos(Ps, CPos), 0, CPos),
                        {'catch', PPos, [st_to_erlfmt(P) || P <- Ps]};
                    _ ->
                        st_to_erlfmt(Pat)
                end;
            {name, Name0} ->
                %% must copy the line from the individual clause
                %% onto the name node
                Name = set_line_from(Clause, Name0),
                Patterns = erl_syntax:clause_patterns(Clause),
                %% have to figure out start and end pos for call node
                NPos = erl_syntax:get_pos(Name),
                % add 1 for start paren
                NPos1 = until(NPos, 1, NPos),
                APos = until(right_pos(Patterns, NPos1), 1, NPos),
                st_to_erlfmt(
                    erl_syntax:set_pos(
                        erl_syntax:application(Name, Patterns),
                        APos
                    )
                );
            'fun' ->
                Patterns = erl_syntax:clause_patterns(Clause),
                %% have to figure out start and end pos for args node
                CPos = erl_syntax:get_pos(Clause),
                % 1 for start paren
                CPos1 = abswidth(1, CPos),
                APos = until(right_pos(Patterns, CPos1), 1, CPos),
                {args, APos, [st_to_erlfmt(P) || P <- Patterns]}
        end,
    Guard =
        case erl_syntax:clause_guard(Clause) of
            none -> empty;
            G -> st_guard_to_erlfmt(G)
        end,
    Body = [st_to_erlfmt(E) || E <- erl_syntax:clause_body(Clause)],
    {clause, Pos, Head, Guard, Body}.

st_guard_to_erlfmt(Guard) ->
    Pos = erl_syntax:get_pos(Guard),
    case erl_syntax:type(Guard) of
        conjunction ->
            {guard_and, Pos, [
                st_guard_to_erlfmt(G)
                || G <- erl_syntax:conjunction_body(Guard)
            ]};
        disjunction ->
            {guard_or, Pos, [
                st_guard_to_erlfmt(G)
                || G <- erl_syntax:disjunction_body(Guard)
            ]};
        _ ->
            st_to_erlfmt(Guard)
    end.

st_typedecl_to_erlfmt({atom, _, type} = Tag, [Term], Pos) ->
    %% the argument is raw erl_parse data here, lifted to abstract form by
    %% erl_syntax, so it first needs to be made concrete again
    {Name, Type, Args} = erl_syntax:concrete(Term),
    Name1 = {atom, dummy_anno(Name), Name},
    Type1 = st_to_erlfmt(Type),
    Args1 = [st_to_erlfmt(A) || A <- Args],
    Def = {op, dummy_anno(), '::', {call, dummy_anno(), Name1, Args1}, Type1},
    {attribute, Pos, Tag, [Def]}.

set_line_from(FromNode, ToNode) ->
    ToPos = set_line_from_1(
        erl_syntax:get_pos(FromNode),
        erl_syntax:get_pos(ToNode)
    ),
    erl_syntax:set_pos(ToNode, ToPos).

set_line_from_1(
    #{location := {L, _}},
    #{location := {_, C1}, end_location := {_, C2}} = ToPos
) ->
    ToPos#{location := {L, C1}, end_location := {L, C2}};
set_line_from_1(#{} = FromPos, ToPos) ->
    %% assume ToPos is not a map, so convert it
    set_line_from_1(FromPos, erlfmt_anno(ToPos)).

until(#{end_location := {L, C}}, N, ToPos) ->
    ToPos#{end_location => {L, C + N}};
until(_FromPos, _N, ToPos) ->
    ToPos.

abswidth(N, #{location := {L, C}} = Pos) ->
    Pos#{end_location => {L, C + N}};
abswidth(_N, Pos) ->
    Pos.

right_pos([], Default) ->
    Default;
right_pos(Nodes, _Default) when is_list(Nodes) ->
    Right = lists:last(Nodes),
    right_pos(Right, erl_syntax:get_pos(Right));
right_pos(Node, Default) ->
    right_pos(
        [N || Group <- erl_syntax:subtrees(Node), N <- Group],
        Default
    ).

%% this would be good to have as a support function in erl_syntax
literal_text(Node) ->
    case erl_syntax:type(Node) of
        atom -> erl_syntax:atom_literal(Node);
        char -> erl_syntax:char_literal(Node);
        float -> erl_syntax:float_literal(Node);
        integer -> erl_syntax:integer_literal(Node);
        operator -> erl_syntax:operator_literal(Node);
        string -> erl_syntax:string_literal(Node);
        text -> erl_syntax:text_literal(Node);
        variable -> erl_syntax:variable_literal(Node);
        _ -> undefined
    end.

%% erlfmt ast utilities

get_anno(Node) ->
    element(2, Node).

set_anno(Node, Loc) ->
    setelement(2, Node, Loc).
