%%==============================================================================
%% The erlang_ls parser. It uses the epp_dodger OTP library.
%%==============================================================================
-module(els_parser).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ parse/1
        , parse_file/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec parse(binary()) -> {ok, [poi()]}.
parse(Text) ->
  IoDevice = els_io_string:new(Text),
  parse_file(IoDevice).

-spec parse_file(file:io_device()) -> {ok, [poi()]}.
parse_file(IoDevice) ->
  {ok, NestedPOIs} = els_dodger:parse(IoDevice, {1, 1}, fun parse_form/3, []),
  ok = file:close(IoDevice),
  {ok, lists:flatten(NestedPOIs)}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% Adapted from els_dodger
-spec parse_form(file:io_device(), any(), [any()]) ->
    {'ok', erl_syntax:forms()
  | none, integer()}
  | {'eof', integer()}
  | {'error', any(), integer()}.
parse_form(IoDevice, Location, Options) ->
  parse_form(IoDevice, Location, fun els_dodger:normal_parser/2, Options).

%% Adapted from els_dodger
-spec parse_form(file:io_device(), any(), function(), [any()]) ->
  {'ok', erl_syntax:forms() | none, integer()}
  | {'eof', integer()}
  | {'error', any(), integer()}.
parse_form(IoDevice, StartLocation, Parser, _Options) ->
  case io:scan_erl_form(IoDevice, "", StartLocation) of
    {ok, Tokens, EndLocation} ->
      try {ok, Parser(Tokens, undefined)} of
        {ok, Tree} ->
          POIs = [ find_attribute_pois(Tree, Tokens)
                 , points_of_interest(Tree, EndLocation)
                 ],
          {ok, POIs, EndLocation}
      catch
        _:_ ->
          {ok, find_attribute_tokens(Tokens), EndLocation}
      end;
    {error, _IoErr, _EndLocation} = Err -> Err;
    {error, _Reason} -> {eof, StartLocation};
    {eof, _EndLocation} = Eof -> Eof
  end.

%% @doc Find POIs in attributes additionally using tokens to add location info
%% missing from the syntax tree. Other attributes which don't need tokens are
%% processed in `attribute/1'.
-spec find_attribute_pois(erl_syntax:syntaxTree(), [erl_scan:token()]) ->
   [poi()].
find_attribute_pois(Tree, Tokens) ->
  case erl_syntax:type(Tree) of
    attribute ->
      try analyze_attribute(Tree) of
        {export, Exports} ->
          %% The first atom is the attribute name, so we skip it.
          [_|Atoms] = [T || {atom, _, _} = T <- Tokens],
          ExportEntries =
            [ poi(Pos, export_entry, {F, A})
              || {{F, A}, {atom, Pos, _}} <- lists:zip(Exports, Atoms)
            ],
          [find_attribute_tokens(Tokens), ExportEntries];
        {import, {M, Imports}} ->
          %% The first two atoms are the attribute name and the imported
          %% module, so we skip them.
          [_, _|Atoms] = [T || {atom, _, _} = T <- Tokens],
          [ poi(Pos, import_entry, {M, F, A})
            || {{F, A}, {atom, Pos, _}} <- lists:zip(Imports, Atoms)];
        {spec, {spec, {{F, A}, _FTs}}} ->
          From = erl_syntax:get_pos(Tree),
          To   = erl_scan:location(lists:last(Tokens)),
          Data = pretty_print(Tree),
          [poi({From, To}, spec, {F, A}, Data)];
        {export_type, {export_type, Exports}} ->
          [_ | Atoms] = [T || {atom, _, _} = T <- Tokens],
          ExportTypeEntries =
            [ poi(Pos, export_type_entry, {F, A})
              || {{F, A}, {atom, Pos, _}} <- lists:zip(Exports, Atoms)
            ],
          [find_attribute_tokens(Tokens), ExportTypeEntries];
        {compile, {compile, CompileOpts}} ->
          find_compile_options_pois(CompileOpts, Tokens);
        _ -> []
      catch
        throw:syntax_error ->
          []
      end;
    _ ->
      []
  end.

%% @doc Analyze an attribute node with special handling for type attributes.
%%
%% `erl_syntax_lib:analyze_attribute` can't handle macros in wild attribute
%% arguments. It also handles `callback', `spec', `type' and `opaque' as wild
%% attributes. Therefore `els_dodger' has to handle these forms specially and
%% here we have to adopt to the different output of `els_dodger'.
%%
%% @see els_dodger:subtrees/1
-spec analyze_attribute(tree()) -> {atom(), term()} | preprocessor.
analyze_attribute(Tree) ->
  case attribute_name_atom(Tree) of
    AttrName when AttrName =:= callback;
                  AttrName =:= spec ->
      [ArgTuple] = erl_syntax:attribute_arguments(Tree),
      [FATree | _] = erl_syntax:tuple_elements(ArgTuple),
      Definition = [], %% ignore definition
      %% concrete will throw an error if `FATRee' contains any macro
      try erl_syntax:concrete(FATree) of
        FA ->
          {AttrName, {AttrName, {FA, Definition}}}
      catch _:_ ->
          throw(syntax_error)
      end;
    AttrName when AttrName =:= opaque;
                  AttrName =:= type ->
      [ArgTuple] = erl_syntax:attribute_arguments(Tree),
      [TypeTree, _, ArgsListTree] = erl_syntax:tuple_elements(ArgTuple),
      Definition = [], %% ignore definition
      %% concrete will throw an error if `TyperTree' is a macro
      try erl_syntax:concrete(TypeTree) of
        TypeName ->
          {AttrName, {AttrName, {TypeName,
                                 Definition,
                                 erl_syntax:list_elements(ArgsListTree)}}}
      catch _:_ ->
          throw(syntax_error)
      end;
    _ ->
      erl_syntax_lib:analyze_attribute(Tree)
  end.

-spec find_compile_options_pois([any()] | tuple(), [erl_scan:token()]) ->
        [poi()].
find_compile_options_pois(CompileOpts, Tokens) when is_tuple(CompileOpts) ->
  find_compile_options_pois([CompileOpts], Tokens);
find_compile_options_pois(CompileOpts, Tokens) when is_list(CompileOpts) ->
  Fun = fun({parse_transform, PT}, Acc) ->
            POIs = [poi(erl_syntax:get_pos(T), parse_transform, Name) ||
                     {atom, _, Name} = T <- Tokens, Name =:= PT],
            POIs ++ Acc;
           (_, Acc) ->
            Acc
        end,
  lists:foldl(Fun, [], CompileOpts);
find_compile_options_pois(_CompileOpts, _Tokens) ->
  [].

%% @doc Resolve POI for specific sections
%%
%% These sections are such things as `export' or `spec' attributes, for which
%% we want to detect their start and end, for example to provide different
%% completion items. Using the tokens provides accurate position for the
%% beginning and end for this sections, and can also handle the situations when
%% the code is not parsable.
-spec find_attribute_tokens([erl_scan:token()]) -> [poi()].
find_attribute_tokens([ {'-', Anno}, {atom, _, Name} | [_|_] = Rest])
  when Name =:= export;
       Name =:= export_type ->
  From = erl_anno:location(Anno),
  To = erl_scan:location(lists:last(Rest)),
  [poi({From, To}, Name, From)];
find_attribute_tokens([ {'-', Anno}, {atom, _, spec} | [_|_] = Rest]) ->
  From = erl_anno:location(Anno),
  To = erl_scan:location(lists:last(Rest)),
  [poi({From, To}, spec, undefined)];
find_attribute_tokens(_) ->
  [].

-spec points_of_interest(tree(), erl_anno:location()) -> [poi()].
points_of_interest(Tree, EndLocation) ->
  FoldFun = fun(T, Acc) -> [do_points_of_interest(T, EndLocation) | Acc] end,
  fold(FoldFun, [], Tree).

%% @doc Return the list of points of interest for a given `Tree'.
-spec do_points_of_interest(tree(), erl_anno:location()) -> [poi()].
do_points_of_interest(Tree, EndLocation) ->
  try
    case erl_syntax:type(Tree) of
      application   -> application(Tree);
      attribute     -> attribute(Tree);
      function      -> function(Tree, EndLocation);
      implicit_fun  -> implicit_fun(Tree);
      macro         -> macro(Tree);
      record_access -> record_access(Tree);
      record_expr   -> record_expr(Tree);
      variable      -> variable(Tree);
      atom          -> atom(Tree);
      Type when Type =:= type_application;
                Type =:= user_type_application ->
        type_application(Tree);
      record_type   -> record_type(Tree);
      _             -> []
    end
  catch throw:syntax_error -> []
  end.

-spec application(tree()) -> [poi()].
application(Tree) ->
  case application_mfa(Tree) of
    undefined -> [];
    {F, A} ->
      Pos = erl_syntax:get_pos(Tree),
      case erl_internal:bif(F, A) of
        %% Call to a function from the `erlang` module
        true -> [poi(Pos, application, {erlang, F, A}, #{imported => true})];
        %% Local call
        false -> [poi(Pos, application, {F, A})]
      end;
    MFA ->
      [poi(erl_syntax:get_pos(Tree), application, MFA)]
  end.

-spec application_mfa(tree()) ->
  {module(), atom(), arity()} | {atom(), arity()} | undefined.
application_mfa(Tree) ->
  case erl_syntax_lib:analyze_application(Tree) of
    %% Remote call
    {M, {F, A}} -> {M, F, A};
    {F, A} -> {F, A};
    A when is_integer(A) ->
      %% If the function is not explicitly named (e.g. a variable is
      %% used as the module qualifier or the function name), only the
      %% arity A is returned.
      %% In the special case where the macro `?MODULE` is used as the
      %% module qualifier, we can consider it as a local call.
      Operator = erl_syntax:application_operator(Tree),
      case erl_syntax:type(Operator) of
        module_qualifier -> application_with_variable(Operator, A);
        _                -> undefined
      end
  end.

-spec application_with_variable(tree(), arity()) ->
  {atom(), arity()} | undefined.
application_with_variable(Operator, A) ->
  Module   = erl_syntax:module_qualifier_argument(Operator),
  Function = erl_syntax:module_qualifier_body(Operator),
  case {erl_syntax:type(Module), erl_syntax:type(Function)} of
    %% The usage of the ?MODULE macro as the module name for
    %% fully qualified calls is so common that it is worth a
    %% specific clause.
    {macro, atom} ->
      ModuleName   = node_name(Module),
      FunctionName = node_name(Function),
      case {ModuleName, FunctionName} of
        {'MODULE', F} -> {F, A};
        _ -> undefined
      end;
    _ -> undefined
  end.

-spec attribute(tree()) -> [poi()].
attribute(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  try analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [poi(Pos, behaviour, Behaviour)];
    {behaviour, {behaviour, Behaviour}} ->
      [poi(Pos, behaviour, Behaviour)];
    {callback, {callback, {{F, A}, _}}} ->
      [poi(Pos, callback, {F, A})];
    {module, {Module, _Args}} ->
      [poi(Pos, module, Module)];
    {module, Module} ->
      [poi(Pos, module, Module)];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [poi(Pos, define, define_name(Define))];
        {include, [String]} ->
          [poi(Pos, include, erl_syntax:string_value(String))];
        {include_lib, [String]} ->
          [poi(Pos, include_lib, erl_syntax:string_value(String))];
        _ ->
          []
      end;
    {record, {Record, Fields}} ->
      [poi(Pos, record, Record, Fields) | record_def_fields(Tree, Record)];
    {type, {type, {Type, _, Args}}} ->
      {Line, Col} = Pos,
      [poi({Line, Col + length("type ")}, type_definition,
           {Type, length(Args)}, type_args(Args))];
    {opaque, {opaque, {Type, _, Args}}} ->
      {Line, Col} = Pos,
      [poi({Line, Col + length("opaque ")}, type_definition,
           {Type, length(Args)}, type_args(Args))];
    _ ->
      []
  catch throw:syntax_error ->
      []
  end.

-spec type_args([any()]) -> [{integer(), string()}].
type_args(Args) ->
  [ case erl_syntax:type(T) of
      variable -> {N, erl_syntax:variable_literal(T)};
      _        -> {N, "Type" ++ integer_to_list(N)}
    end
    || {N, T} <- lists:zip(lists:seq(1, length(Args)), Args)
  ].

-spec function(tree(), erl_anno:location()) -> [poi()].
function(Tree, {EndLine, _} = _EndLocation) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  Clauses = erl_syntax:function_clauses(Tree),
  IndexedClauses = lists:zip(lists:seq(1, length(Clauses)), Clauses),
  ClausesPOIs = [ poi( erl_syntax:get_pos(Clause)
                     , function_clause
                     , {F, A, I}
                     , pretty_print_clause(Clause)
                     )
                  || {I, Clause} <- IndexedClauses],
  Args = function_args(hd(Clauses), A),
  {StartLine, _} = StartLocation = erl_syntax:get_pos(Tree),
  %% It only makes sense to fold a function if the function contains
  %% at least one line apart from its signature.
  FoldingRanges = case EndLine - StartLine > 1 of
                    true ->
                      Range = #{ from => {StartLine, ?END_OF_LINE}
                               , to   => {EndLine - 1, ?END_OF_LINE}
                               },
                      [ els_poi:new(Range, folding_range, StartLocation) ];
                    false ->
                      []
                  end,
  lists:append([ [ poi(StartLocation, function, {F, A}, Args) ]
               , FoldingRanges
               , ClausesPOIs
               ]).

-spec function_args(tree(), arity()) -> [{integer(), string()}].
function_args(Clause, Arity) ->
  Patterns = erl_syntax:clause_patterns(Clause),
  [ case erl_syntax:type(P) of
      %% TODO: Handle literals
      variable -> {N, erl_syntax:variable_literal(P)};
      _        -> {N, "Arg" ++ integer_to_list(N)}
    end
    || {N, P} <- lists:zip(lists:seq(1, Arity), Patterns)
  ].

-spec implicit_fun(tree()) -> [poi()].
implicit_fun(Tree) ->
  FunSpec = try erl_syntax_lib:analyze_implicit_fun(Tree) of
              {M, {F, A}} -> {M, F, A};
              {F, A} -> {F, A}
            catch throw:syntax_error ->
                undefined
            end,
  case FunSpec of
    undefined -> [];
    _ -> [poi(erl_syntax:get_pos(Tree), implicit_fun, FunSpec)]
  end.

-spec macro(tree()) -> [poi()].
macro(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [poi(Pos, macro, node_name(Tree))]
  end.

-spec record_access(tree()) -> [poi()].
record_access(Tree) ->
  RecordNode = erl_syntax:record_access_type(Tree),
  FieldNode = erl_syntax:record_access_field(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_value(RecordNode),
      FieldPoi =
        case erl_syntax:type(FieldNode) of
          atom ->
            Field = erl_syntax:atom_value(FieldNode),
            [poi(erl_syntax:get_pos(FieldNode), record_field, {Record, Field})];
          _    ->
            []
        end,
      [ poi(erl_syntax:get_pos(Tree), record_expr, Record)
      | FieldPoi ];
    _ ->
      []
  end.

-spec record_expr(tree()) -> [poi()].
record_expr(Tree) ->
  RecordNode = erl_syntax:record_expr_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_value(RecordNode),
      FieldPois  = lists:append(
                     [record_field_name(F, Record, record_field)
                      || F <- erl_syntax:record_expr_fields(Tree)]),
      [ poi(erl_syntax:get_pos(Tree), record_expr, Record)
      | FieldPois ];
    _ ->
      []
  end.

-spec record_field_name(tree(), atom(), poi_kind()) -> [poi()].
record_field_name(FieldNode, Record, Kind) ->
  NameNode =
    case erl_syntax:type(FieldNode) of
      record_field ->
        erl_syntax:record_field_name(FieldNode);
      record_type_field ->
        erl_syntax:record_type_field_name(FieldNode)
    end,
  case erl_syntax:type(NameNode) of
    atom ->
      Pos = erl_syntax:get_pos(NameNode),
      NameAtom = erl_syntax:atom_value(NameNode),
      [poi(Pos, Kind, {Record, NameAtom})];
    _ ->
      []
  end.

-spec record_def_fields(tree(), atom()) -> [poi()].
record_def_fields(AttrTree, Record) ->
  case erl_syntax:attribute_arguments(AttrTree) of
    none -> [];
    [_R, T] ->
      case erl_syntax:type(T) of
        tuple ->
          lists:append(
            [record_def_field(F, Record)
             || F <- erl_syntax:tuple_elements(T)]);
        _ ->
          []
      end
  end.

-spec record_def_field(tree(), atom()) -> [poi()].
record_def_field(FieldTree, Record) ->
  case erl_syntax:type(FieldTree) of
    record_field ->
      record_field_name(FieldTree, Record, record_def_field);
    typed_record_field ->
      F = erl_syntax:typed_record_field_body(FieldTree),
      record_field_name(F, Record, record_def_field);
    _ ->
      []
  end.

-spec record_type(tree()) -> [poi()].
record_type(Tree) ->
  RecordNode = erl_syntax:record_type_name(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_value(RecordNode),
      FieldPois  = lists:append(
                     [record_field_name(F, Record, record_field)
                      || F <- erl_syntax:record_type_fields(Tree)]),
      [ poi(erl_syntax:get_pos(Tree), record_expr, Record)
      | FieldPois ];
    _ ->
      []
  end.

-spec type_application(tree()) -> [poi()].
type_application(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  Type = erl_syntax:type(Tree),
  case erl_syntax_lib:analyze_type_application(Tree) of
    {Module, {Name, Arity}} ->
      %% remote type
      Id = {Module, Name, Arity},
      [poi(Pos, type_application, Id)];
    {Name, Arity} when Type =:= user_type_application ->
      %% user-defined local type
      Id = {Name, Arity},
      [poi(Pos, type_application, Id)];
    {_Name, _Arity} when Type =:= type_application  ->
      %% No POIs for built-in types
      []
  end.

-spec variable(tree()) -> [poi()].
variable(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [poi(Pos, variable, node_name(Tree))]
  end.

-spec atom(tree()) -> [poi()].
atom(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [poi(Pos, atom, node_name(Tree))]
  end.

-spec define_name(tree()) -> atom().
define_name(Tree) ->
  case erl_syntax:type(Tree) of
    application ->
      Operator = erl_syntax:application_operator(Tree),
      node_name(Operator);
    variable ->
      erl_syntax:variable_name(Tree);
    atom ->
      erl_syntax:atom_value(Tree);
    underscore ->
      '_'
  end.

-spec node_name(tree()) -> atom().
node_name(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      erl_syntax:atom_value(Tree);
    variable ->
      erl_syntax:variable_name(Tree);
    macro ->
      node_name(erl_syntax:macro_name(Tree));
    underscore ->
      '_'
  end.

-spec poi(pos() | {pos(), pos()}, poi_kind(), any()) -> poi().
poi(Pos, Kind, Id) ->
  poi(Pos, Kind, Id, undefined).

-spec poi(pos() | {pos(), pos()}, poi_kind(), any(), any()) ->
  poi().
poi(Pos, Kind, Id, Data) ->
  Range = els_range:range(Pos, Kind, Id, Data),
  els_poi:new(Range, Kind, Id, Data).

%% @doc Fold over nodes in the tree
%%
%% Modified version of `erl_syntax_lib:fold/3', to get control over
%% what subtrees should be folded over for certain types of nodes.
-spec fold(fun((tree(), term()) -> term()), term(), tree()) -> term().
fold(F, S, Tree) ->
  case subtrees(Tree, erl_syntax:type(Tree)) of
    [] -> F(Tree, S);
    Gs -> F(Tree, fold_1(F, S, Gs))
  end.

-spec fold_1(fun((tree(), term()) -> term()), term(), [[tree()]]) ->
  term().
fold_1(F, S, [L | Ls]) ->
  fold_1(F, fold_2(F, S, L), Ls);
fold_1(_, S, []) ->
  S.

-spec fold_2(fun((tree(), term()) -> term()), term(), [tree()]) ->
  term().
fold_2(F, S, [T | Ts]) ->
  fold_2(F, fold(F, S, T), Ts);
fold_2(_, S, []) ->
  S.

-spec subtrees(tree(), atom()) -> [[tree()]].
subtrees(Tree, application) ->
  [erl_syntax:application_arguments(Tree)];
subtrees(Tree, function) ->
  [erl_syntax:function_clauses(Tree)];
subtrees(_Tree, implicit_fun) ->
  [];
subtrees(Tree, macro) ->
  case erl_syntax:macro_arguments(Tree) of
    none -> [];
    Args -> [Args]
  end;
subtrees(Tree, record_access) ->
  NameNode = erl_syntax:record_access_field(Tree),
  [ [erl_syntax:record_access_argument(Tree)]
  , skip_record_field_atom(NameNode)
  ];
subtrees(Tree, record_expr) ->
  Fields = erl_syntax:record_expr_fields(Tree),
  case erl_syntax:record_expr_argument(Tree) of
    none -> [Fields];
    Arg  -> [[Arg], Fields]
  end;
subtrees(Tree, record_field) ->
  NameNode = erl_syntax:record_field_name(Tree),
  [ skip_record_field_atom(NameNode)
  , case erl_syntax:record_field_value(Tree) of
      none ->
        [];
      V ->
       [V]
    end];
subtrees(Tree, record_type) ->
  NameNode = erl_syntax:record_type_name(Tree),
  [ skip_record_field_atom(NameNode)
  , erl_syntax:record_type_fields(Tree)
  ];
subtrees(Tree, record_type_field) ->
  NameNode = erl_syntax:record_type_field_name(Tree),
  [ skip_record_field_atom(NameNode)
  , [erl_syntax:record_type_field_type(Tree)]
  ];
subtrees(Tree, user_type_application) ->
  NameNode = erl_syntax:user_type_application_name(Tree),
  [ skip_record_field_atom(NameNode)
  , erl_syntax:user_type_application_arguments(Tree)
  ];
subtrees(Tree, type_application) ->
  NameNode = erl_syntax:type_application_name(Tree),
  [ skip_type_name_atom(NameNode)
  , erl_syntax:type_application_arguments(Tree)
  ];
subtrees(Tree, attribute) ->
  AttrName = attribute_name_atom(Tree),
  Args = case erl_syntax:attribute_arguments(Tree) of
           none -> [];
           Args0 -> Args0
         end,
  attribute_subtrees(AttrName, Args);
subtrees(Tree, _) ->
  erl_syntax:subtrees(Tree).

-spec attribute_name_atom(tree()) -> atom() | tree().
attribute_name_atom(Tree) ->
  NameNode = erl_syntax:attribute_name(Tree),
  case erl_syntax:type(NameNode) of
    atom ->
      erl_syntax:atom_value(NameNode);
    _ ->
      NameNode
  end.

-spec attribute_subtrees(atom() | tree(), [tree()]) -> [[tree()]].
attribute_subtrees(AttrName, [Mod])
  when AttrName =:= module;
       AttrName =:= behavior;
       AttrName =:= behaviour ->
  [skip_record_field_atom(Mod)];
attribute_subtrees(record, [_RecordName, FieldsTuple]) ->
  [[FieldsTuple]];
attribute_subtrees(import, [Mod, Imports]) ->
  [ skip_record_field_atom(Mod)
  , [Imports]];
attribute_subtrees(define, [_Name | Definition]) ->
  %% The definition can contain commas, in which case it will look like as if
  %% the attribute would have more than two arguments. Eg.: `-define(M, a, b).'
  [Definition];
attribute_subtrees(AttrName, _)
  when AttrName =:= include;
       AttrName =:= include_lib ->
  [];
attribute_subtrees(AttrName, Args)
  when is_atom(AttrName) ->
      [Args];
attribute_subtrees(AttrName, Args) ->
  %% Attribute name not an atom, probably a macro
  [[AttrName], Args].

%% Skip visiting atoms of record field names as they are already represented as
%% `record_field' pois
-spec skip_record_field_atom(tree()) -> [tree()].
skip_record_field_atom(NameNode) ->
  case erl_syntax:type(NameNode) of
     atom ->
       [];
     _ ->
       [NameNode]
   end.

-spec skip_type_name_atom(tree()) -> [tree()].
skip_type_name_atom(NameNode) ->
  case erl_syntax:type(NameNode) of
     atom ->
       [];
    module_qualifier ->
      skip_record_field_atom(erl_syntax:module_qualifier_body(NameNode))
        ++
        skip_record_field_atom(erl_syntax:module_qualifier_argument(NameNode));
     _ ->
       [NameNode]
   end.
-spec pretty_print(tree()) -> binary().
pretty_print(Tree) ->
  try
    els_utils:to_binary(erl_prettypr:format(Tree))
 catch
   _:_:_ ->
     <<>>
 end.

-spec pretty_print_clause(tree()) -> binary().
pretty_print_clause(Tree) ->
  Patterns = erl_syntax:clause_patterns(Tree),
  PrettyPatterns = [ erl_prettypr:format(P) || P <- Patterns],
  Guard = erl_syntax:clause_guard(Tree),
  PrettyGuard = case Guard of
                  none ->
                    "";
                  _ ->
                    "when " ++ erl_prettypr:format(Guard)
                end,
  PrettyClause = io_lib:format( "(~s) ~s"
                              , [ string:join(PrettyPatterns, ", ")
                                , PrettyGuard
                                ]),
  els_utils:to_binary(PrettyClause).
