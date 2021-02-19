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

-export([erlfmt_to_st/1]).

%% syntax_tree -> erlfmt conversion is not used in erlang_ls,
%% removed to not have to update it with erlang_ls custom changes and
%% fix dialyzer warnings about erl_syntax:get_pos/set_pos
%%-export([st_to_erlfmt/1]).

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
  Context = get('$erlfmt_ast_context$'),
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
                        %% Convert field types in a type context
                        put('$erlfmt_ast_context$', type),
                        T1 = erlfmt_to_st(T),
                        erase('$erlfmt_ast_context$'),
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
      {attribute, Pos, {atom, _, Tag} = Name, [Def]} when Tag =:= type; Tag =:= opaque ->
        put('$erlfmt_ast_context$', type),
        {op, OPos, '::', Type, Definition} = Def,
        {TypeName, Args} =
          case Type of
            {call, _CPos, TypeName0, Args0} ->
              {TypeName0, Args0};
            {macro_call, CPos, {_, MPos, _} = MacroName, Args0} ->
              EndLoc = maps:get(end_location, MPos),
              TypeName0 = {macro_call, CPos#{end_location => EndLoc}, MacroName, none},
              {TypeName0, Args0}
          end,
        Tree =
          erl_syntax:set_pos(
            erl_syntax:attribute(erlfmt_to_st(Name),
                                 [erl_syntax:set_pos(
                                    erl_syntax:tuple([erlfmt_to_st(TypeName),
                                                      erlfmt_to_st(Definition),
                                                      erl_syntax:list([erlfmt_to_st(A) || A <- Args])]),
                                    OPos)]),
            Pos),
        erase('$erlfmt_ast_context$'),
        Tree;
      {attribute, Pos, {atom, _, RawName} = Name, Args} when RawName =:= callback;
                                                             RawName =:= spec ->
        put('$erlfmt_ast_context$', type),
        [{spec, SPos, FName, Clauses}] = Args,
        {spec_clause, _, {args, _, ClauseArgs}, _, _} = hd(Clauses),
        Arity = length(ClauseArgs),
        Tree =
          erl_syntax:set_pos(
            erl_syntax:attribute(erlfmt_to_st(Name),
                                 [erl_syntax:set_pos(
                                    erl_syntax:tuple([erl_syntax:tuple([erlfmt_to_st(FName), erl_syntax:integer(Arity)]),
                                                      erl_syntax:list([erlfmt_to_st(C) || C <- Clauses])]),
                                       SPos)]),
            Pos),
        erase('$erlfmt_ast_context$'),
        Tree;
      {spec_clause, Pos, {args, _HeadMeta, Args}, [ReturnType], empty} ->
        erl_syntax:set_pos(
          erl_syntax_function_type([erlfmt_to_st(A) || A <- Args],
                                   erlfmt_to_st(ReturnType)),
          Pos);
      {spec_clause, Pos, {args, _HeadMeta, Args}, [ReturnType], GuardOr} ->
        FunctionType =
          erl_syntax:set_pos(
            erl_syntax_function_type([erlfmt_to_st(A) || A <- Args],
                                     erlfmt_to_st(ReturnType)),
            Pos),
        FunctionConstraint = erlfmt_guard_to_st(GuardOr),

        erl_syntax:set_pos(
          erl_syntax:constrained_function_type(FunctionType, [FunctionConstraint]),
          Pos);
      {op, Pos, '|', A, B} when Context =:= type ->
        erl_syntax:set_pos(
          erl_syntax:type_union([erlfmt_to_st(A),
                                 erlfmt_to_st(B)]),
          Pos);
      {op, Pos, '..', A, B} when Context =:= type ->
        %% erlfmt_to_st_1({type, Pos, range, [A, B]}),
        erl_syntax:set_pos(
          erl_syntax:integer_range_type(erlfmt_to_st(A),
                                        erlfmt_to_st(B)),
          Pos);
      %%{op, Pos, '::', A, B} when Context =:= type ->
      %%  erl_syntax:set_pos(
      %%    erl_syntax:annotated_type(erlfmt_to_st(A),
      %%                              erlfmt_to_st(B)),
      %%    Pos);
      {record, Pos, Name, Fields} when Context =:= type ->
        %% The record name is represented as node instead of a raw atom
        %% and typed record fields are represented as '::' ops
        Fields1 = [
                   case F of
                     {op, FPos, '::', B, T} ->
                       B1 = erlfmt_to_st(B),
                       T1 = erlfmt_to_st(T),
                       erl_syntax:set_pos(
                         erl_syntax:record_type_field(B1, T1),
                         FPos
                        );
                     _ ->
                       erlfmt_to_st(F)
                   end
                   || F <- Fields
                  ],

        erl_syntax:set_pos(
          erl_syntax:record_type(
            erlfmt_to_st(Name),
            Fields1
           ),
          Pos
         );
      {call, Pos, {remote, _, _, _} = Name, Args} when Context =:= type ->
        erl_syntax:set_pos(
          erl_syntax:type_application(erlfmt_to_st(Name),
                                      [erlfmt_to_st(A) || A <- Args]),
          Pos);
      {call, Pos, Name, Args} when Context =:= type ->
        TypeTag =
          case Name of
            {atom, _, NameAtom} ->
              Arity = length(Args),
              case erl_internal:is_type(NameAtom, Arity) of
                true ->
                  type_application;
                false ->
                  user_type_application
              end;
            _ ->
              user_type_application
          end,
        erl_syntax:set_pos(
          erl_syntax:TypeTag(erlfmt_to_st(Name),
                             [erlfmt_to_st(A) || A <- Args]),
          Pos);
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
      {'fun', Pos, type} ->
        erl_syntax:set_pos(erl_syntax:fun_type(), Pos);
      {'fun', Pos, {type, _, {args, _, Args}, Res}} ->
        erl_syntax:set_pos(
          erl_syntax_function_type(
            [erlfmt_to_st(A) || A <- Args],
            erlfmt_to_st(Res)),
          Pos);
      {'bin', Pos, Elements} when Context =:= type ->
        %% Note: we loose a lot of Annotation info here
        %% Note2: erl_parse assigns the line number (with no column) to the dummy zeros
        {M, N} =
          case Elements of
            [{bin_element, _, {var, _, '_'}, {bin_size, _, {var, _, '_'}, NNode}, default}] ->
              {{integer, dummy_anno(), 0}, NNode};
            [{bin_element, _, {var, _, '_'}, MNode, default}] ->
              {MNode, {integer, dummy_anno(), 0}};
            [{bin_element, _, {var, _, '_'}, MNode, default},
             {bin_element, _, {var, _, '_'}, {bin_size, _, {var, _, '_'}, NNode}, default}] ->
              {MNode, NNode};
            [] ->
              {{integer, dummy_anno(), 0}, {integer, dummy_anno(), 0}};
            _ ->
              %% No idea what this is - what ST should we create?
              %% maybe just a binary(), or an empty text node
              {{integer, dummy_anno(), 0}, {integer, dummy_anno(), 1}}
          end,
        erl_syntax:set_pos(
          erl_syntax:bitstring_type(
            erlfmt_to_st(M),
            erlfmt_to_st(N)),
          Pos);
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
        {guard_or, _Pos, _Exprs} ->
            erlfmt_guard_to_st(Node);
        {guard_and, _Pos, _Exprs} ->
            erlfmt_guard_to_st(Node);
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
-spec erlfmt_to_st_1(_) -> any().
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

-spec erlfmt_subtrees_to_st([[any()]]) -> [[any()]].
erlfmt_subtrees_to_st(Groups) ->
    [
        [
            erlfmt_to_st(Subtree)
            || Subtree <- Group
        ]
        || Group <- Groups
    ].

-spec get_function_name(maybe_improper_list()) -> any().
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
-spec erlfmt_clause_to_st(_) -> any().
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

-spec erlfmt_clause_to_st(_,[any()],_,[any()]) -> any().
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

-spec erlfmt_guard_to_st(_) -> any().
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

-spec fold_arity_qualifiers(_) -> any().
fold_arity_qualifiers(Tree) ->
    erl_syntax_lib:map(fun fold_arity_qualifier/1, Tree).

-spec fold_arity_qualifier(_) -> any().
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

-spec dummy_anno() -> map().
dummy_anno() ->
    erlfmt_anno(0).

%% assumes that if the annotation is a map, it came from erlfmt_scan
-spec erlfmt_anno(_) -> map().
erlfmt_anno(Map) when is_map(Map) ->
    Map;
erlfmt_anno(Line) ->
    %% have to add end_location as well even if it's incorrect
    #{
        location => {Line, 1},
        % TODO: improve this
        end_location => {Line, 1}
    }.

%% erlfmt ast utilities

-spec get_anno(tuple()) -> any().
get_anno(Node) ->
    element(2, Node).

-spec set_anno(tuple(),map()) -> tuple().
set_anno(Node, Loc) ->
    setelement(2, Node, Loc).

%% @doc Silence warning about breaking the contract
%% erl_syntax:function_type/2 has wrong spec before OTP 24
-spec erl_syntax_function_type('any_arity' | [syntax_tools()], syntax_tools()) -> syntax_tools().
erl_syntax_function_type(Arguments, Return) ->
  apply(erl_syntax, function_type, [Arguments, Return]).
