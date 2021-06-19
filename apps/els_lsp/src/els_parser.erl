%%==============================================================================
%% The erlang_ls parser. It uses the epp_dodger OTP library.
%%==============================================================================
-module(els_parser).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ parse/1
        , parse_file/1
        , parse_text/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-type deep_list(T) :: [T | deep_list(T)].

%%==============================================================================
%% API
%%==============================================================================
-spec parse(binary()) -> {ok, [poi()]}.
parse(Text) ->
  String = els_utils:to_list(Text),
  case erlfmt:read_nodes_string("nofile", String) of
    {ok, Forms, _ErrorInfo} ->
      {ok, lists:flatten(parse_forms(Forms))};
    {error, _ErrorInfo} ->
      {ok, []}
  end.

-spec parse_file(file:name_all()) -> {ok, [tree()]} | {error, term()}.
parse_file(FileName) ->
  forms_to_ast(erlfmt:read_nodes(FileName)).

-spec parse_text(binary()) -> {ok, [tree()]} | {error, term()}.
parse_text(Text) ->
  String = els_utils:to_list(Text),
  forms_to_ast(erlfmt:read_nodes_string("nofile", String)).

-spec forms_to_ast(tuple()) -> {ok, [tree()]} | {error, term()}.
forms_to_ast({ok, Forms, _ErrorInfo}) ->
  TreeList =
    [els_erlfmt_ast:erlfmt_to_st(Form) || Form <- Forms],
  {ok, TreeList};
forms_to_ast({error, _ErrorInfo} = Error) ->
  Error.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec parse_forms([erlfmt_parse:abstract_node()]) -> deep_list(poi()).
parse_forms(Forms) ->
  [try
     parse_form(Form)
   catch Type:Reason:St ->
       ?LOG_WARNING("Please report error parsing form ~p:~p:~p~n~p~n",
                    [Type, Reason, St, Form]),
       []
   end
   || Form <- Forms].

-spec parse_form(erlfmt_parse:abstract_node()) -> deep_list(poi()).
parse_form({raw_string, Anno, Text}) ->
  Start = erlfmt_scan:get_anno(location, Anno),
  {ok, RangeTokens, _EndLocation} = erl_scan:string(Text, Start, [text]),
  find_attribute_tokens(RangeTokens);
parse_form(Form) ->
  Tree = els_erlfmt_ast:erlfmt_to_st(Form),
  POIs = points_of_interest(Tree),
  POIs.

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
  To = token_end_location(lists:last(Rest)),
  [poi({From, To}, Name, From)];
find_attribute_tokens([ {'-', Anno}, {atom, _, spec} | [_|_] = Rest]) ->
  From = erl_anno:location(Anno),
  To = token_end_location(lists:last(Rest)),
  [poi({From, To}, spec, undefined)];
find_attribute_tokens(_) ->
  [].

%% Inspired by erlfmt_scan:dot_anno
-spec token_end_location(erl_scan:token()) -> erl_anno:location().
token_end_location({dot, Anno}) ->
  %% Special handling for dot tokens, which by definition contain a dot char
  %% followed by a whitespace char. We don't want to count the whitespace (which
  %% is usually a newline) as part of the form.
  {Line, Col} = erl_anno:location(Anno),
  {Line, Col + 1};
token_end_location(Token) ->
  erl_scan:end_location(Token).

-spec points_of_interest(tree()) -> [[poi()]].
points_of_interest(Tree) ->
  FoldFun = fun(T, Acc) -> [do_points_of_interest(T) | Acc] end,
  fold(FoldFun, [], Tree).

%% @doc Return the list of points of interest for a given `Tree'.
-spec do_points_of_interest(tree()) -> [poi()].
do_points_of_interest(Tree) ->
  try
    case erl_syntax:type(Tree) of
      application   -> application(Tree);
      attribute     -> attribute(Tree);
      function      -> function(Tree);
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
      Pos = erl_syntax:get_pos(erl_syntax:application_operator(Tree)),
      case erl_internal:bif(F, A) of
        %% Call to a function from the `erlang` module
        true -> [poi(Pos, application, {erlang, F, A}, #{imported => true})];
        %% Local call
        false -> [poi(Pos, application, {F, A})]
      end;
    MFA ->
      Pos = erl_syntax:get_pos(erl_syntax:application_operator(Tree)),
      [poi(Pos, application, MFA)]
  end.

-spec application_mfa(tree()) ->
  {module(), atom(), arity()} | {atom(), arity()} | undefined.
application_mfa(Tree) ->
  case erl_syntax_lib:analyze_application(Tree) of
    %% Remote call
    {M, {F, A}} ->
      {M, F, A};
    {F, A} ->
      {F, A};
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
      ModuleName   = macro_name(Module),
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
  try {attribute_name_atom(Tree), erl_syntax:attribute_arguments(Tree)} of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {AttrName, [Arg]} when AttrName =:= behaviour;
                           AttrName =:= behavior ->
      case is_atom_node(Arg) of
        {true, Behaviour} ->
          [poi(Pos, behaviour, Behaviour)];
        false ->
          []
      end;
    {module, [Module, _Args]} ->
      case is_atom_node(Module) of
        {true, ModuleName} ->
          [poi(erl_syntax:get_pos(Module), module, ModuleName)];
        _ ->
          []
      end;
    {module, [Module]} ->
      case is_atom_node(Module) of
        {true, ModuleName} ->
          [poi(erl_syntax:get_pos(Module), module, ModuleName)];
        _ ->
          []
      end;
    {compile, [Arg]} ->
      find_compile_options_pois(Arg);
    {AttrName, [Arg]} when AttrName =:= export;
                           AttrName =:= export_type ->
      find_export_pois(Tree, AttrName, Arg);
    {import, [ModTree, ImportList]} ->
      case is_atom_node(ModTree) of
        {true, M} ->
          Imports = erl_syntax:list_elements(ImportList),
          find_import_entry_pois(M, Imports);
        _ ->
          []
      end;
    {define, [Define|Value]} ->
      DefinePos = case erl_syntax:type(Define) of
                    application ->
                      Operator = erl_syntax:application_operator(Define),
                      erl_syntax:get_pos(Operator);
                    _ ->
                      erl_syntax:get_pos(Define)
                  end,
      ValueRange = #{ from => get_start_location(hd(Value))
                    , to => get_end_location(lists:last(Value))
                    },
      Args = define_args(Define),
      Data = #{value_range => ValueRange, args => Args},
      [poi(DefinePos, define, define_name(Define), Data)];
    {include, [String]} ->
      [poi(Pos, include, erl_syntax:string_value(String))];
    {include_lib, [String]} ->
      [poi(Pos, include_lib, erl_syntax:string_value(String))];
    {record, [Record, Fields]} ->
      case is_atom_node(Record) of
        {true, RecordName} ->
          %% FIXME clean FieldList up -> analyze_record_fields
          FieldList =
            lists:flatten(
              [case erl_syntax:type(F) of
                 record_field ->
                   FieldName = erl_syntax:record_field_name(F),
                   {erl_syntax:atom_value(FieldName),
                    erl_syntax:record_field_value(F)};
                 typed_record_field ->
                   FF = erl_syntax:typed_record_field_body(F),
                   FieldName = erl_syntax:record_field_name(FF),
                   {erl_syntax:atom_value(FieldName),
                    erl_syntax:record_field_value(FF)}
               end
               || F <- erl_syntax:tuple_elements(Fields)]),
          ValueRange = #{ from => get_start_location(Tree),
                          to => get_end_location(Tree)},
          Data = #{field_list => FieldList, value_range => ValueRange},
          [poi(erl_syntax:get_pos(Record), record, RecordName, Data)
          | record_def_fields(Tree, RecordName)];
        _ ->
          []
      end;
    {AttrName, [ArgTuple]} when AttrName =:= type;
                                AttrName =:= opaque ->
      [Type, _, ArgsListTree] = erl_syntax:tuple_elements(ArgTuple),
      TypeArgs = erl_syntax:list_elements(ArgsListTree),
      case is_atom_node(Type) of
        {true, TypeName} ->
          [poi(erl_syntax:get_pos(Type), type_definition,
               {TypeName, length(TypeArgs)}, type_args(TypeArgs))];
        _ ->
          []
      end;
    {callback, [ArgTuple]} ->
      [FATree | _] = erl_syntax:tuple_elements(ArgTuple),
      case spec_function_name(FATree) of
        {F, A} ->
          [FTree, _] = erl_syntax:tuple_elements(FATree),
          Anno = erl_syntax:get_pos(FTree),
          %% FIXME this is weird:
          %% starts at '-', ends at the end of callback function name
          Start = get_start_location(Tree),
          CallbackAnno = erl_anno:set_location(Start, Anno),
          [poi(CallbackAnno, callback, {F, A})];
        undefined ->
          []
      end;
    {spec, [ArgTuple]} ->
      [FATree | _] = erl_syntax:tuple_elements(ArgTuple),
      case spec_function_name(FATree) of
        {F, A} ->
          [poi(Pos, spec, {F, A})];
        undefined ->
          [poi(Pos, spec, undefined)]
      end;
    _ ->
      []
  catch throw:syntax_error ->
      []
  end.

-spec find_compile_options_pois(tree()) -> [poi()].
find_compile_options_pois(Arg) ->
  case erl_syntax:type(Arg) of
    list ->
      L = erl_syntax:list_elements(Arg),
      lists:flatmap(fun find_compile_options_pois/1, L);
    tuple ->
      case erl_syntax:tuple_elements(Arg) of
        [K, V] ->
          case {is_atom_node(K), is_atom_node(V)} of
            {{true, parse_transform}, {true, PT}} ->
              [poi(erl_syntax:get_pos(V), parse_transform, PT)];
            _ ->
              []
          end;
        _ ->
          []
      end;
    atom ->
      %% currently there is no atom compile option that we are interested in
      [];
    _ ->
      []
  end.

-spec find_export_pois(tree(), export | export_type, tree()) -> [poi()].
find_export_pois(Tree, AttrName, Arg) ->
  Exports = erl_syntax:list_elements(Arg),
  EntryPoiKind = case AttrName of
                   export      -> export_entry;
                   export_type -> export_type_entry
                 end,
  ExportEntries = find_export_entry_pois(EntryPoiKind, Exports),
  [ poi(erl_syntax:get_pos(Tree), AttrName, get_start_location(Tree))
  | ExportEntries ].

-spec find_export_entry_pois(export_entry | export_type_entry, [tree()])
                            -> [poi()].
find_export_entry_pois(EntryPoiKind, Exports) ->
  lists:flatten(
    [ case get_name_arity(FATree) of
        {F, A} ->
          poi(erl_syntax:get_pos(FATree), EntryPoiKind, {F, A});
        false ->
          []
      end
      || FATree <- Exports
    ]).

-spec find_import_entry_pois(atom(), [tree()]) -> [poi()].
find_import_entry_pois(M, Imports) ->
  lists:flatten(
    [ case get_name_arity(FATree) of
        {F, A} ->
          poi(erl_syntax:get_pos(FATree), import_entry, {M, F, A});
        false ->
          []
      end
      || FATree <- Imports
    ]).

-spec spec_function_name(tree()) -> {atom(), arity()} | undefined.
spec_function_name(FATree) ->
  %% concrete will throw an error if `FATree' contains any macro
  try erl_syntax:concrete(FATree) of
    {F, A} -> {F, A};
    _ -> undefined
  catch _:_ ->
      undefined
  end.

-spec type_args([tree()]) -> [{integer(), string()}].
type_args(Args) ->
  [ case erl_syntax:type(T) of
      variable -> {N, erl_syntax:variable_literal(T)};
      _        -> {N, "Type" ++ integer_to_list(N)}
    end
    || {N, T} <- lists:zip(lists:seq(1, length(Args)), Args)
  ].

-spec function(tree()) -> [poi()].
function(Tree) ->
  FunName = erl_syntax:function_name(Tree),
  Clauses = erl_syntax:function_clauses(Tree),
  {F, A, Args} = analyze_function(FunName, Clauses),

  IndexedClauses = lists:zip(lists:seq(1, length(Clauses)), Clauses),
  %% FIXME function_clause range should be the range of the name atom however
  %% that is not present in the clause Tree (it is in the erlfmt_parse node)
  ClausesPOIs = [ poi( get_start_location(Clause)
                     , function_clause
                     , {F, A, I}
                     , pretty_print_clause(Clause)
                     )
                  || {I, Clause} <- IndexedClauses,
                     erl_syntax:type(Clause) =:= clause],
  {StartLine, _} = StartLocation = get_start_location(Tree),
  {EndLine, _} = get_end_location(Tree),
  %% It only makes sense to fold a function if the function contains
  %% at least one line apart from its signature.
  FoldingRanges = case EndLine > StartLine of
                    true ->
                      Range = #{ from => {StartLine, ?END_OF_LINE}
                               , to   => {EndLine, ?END_OF_LINE}
                               },
                      [ els_poi:new(Range, folding_range, StartLocation) ];
                    false ->
                      []
                  end,
  lists:append([ [ poi(erl_syntax:get_pos(FunName), function, {F, A}, Args) ]
               , FoldingRanges
               , ClausesPOIs
               ]).

-spec analyze_function(tree(), [tree()]) ->
        {atom(), arity(), [{integer(), string()}]}.
analyze_function(FunName, Clauses) ->
  F = case is_atom_node(FunName) of
        {true, FAtom} -> FAtom;
        false -> throw(syntax_error)
      end,

  case lists:dropwhile(fun(T) -> erl_syntax:type(T) =/= clause end, Clauses) of
    [Clause | _] ->
      {Arity, Args} = function_args(Clause),
      {F, Arity, Args};
    [] ->
      throw(syntax_error)
  end.

-spec function_args(tree()) -> {arity(), [{integer(), string()}]}.
function_args(Clause) ->
  Patterns = erl_syntax:clause_patterns(Clause),
  Arity = length(Patterns),
  Args = args_from_subtrees(Patterns),
  {Arity, Args}.


-spec args_from_subtrees([tree()]) -> [{integer(), string()}].
args_from_subtrees(Trees) ->
  Arity = length(Trees),
  [ case erl_syntax:type(T) of
      %% TODO: Handle literals
      variable -> {N, erl_syntax:variable_literal(T)};
      _        -> {N, "Arg" ++ integer_to_list(N)}
    end
    || {N, T} <- lists:zip(lists:seq(1, Arity), Trees)
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
  Anno = macro_location(Tree),
  [poi(Anno, macro, macro_name(Tree))].

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
      Anno = record_access_location(Tree),
      [ poi(Anno, record_expr, Record)
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
      Anno = record_expr_location(Tree, RecordNode),
      [ poi(Anno, record_expr, Record)
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
      Anno = record_expr_location(Tree, RecordNode),
      [ poi(Anno, record_expr, Record)
      | FieldPois ];
    _ ->
      []
  end.

-spec type_application(tree()) -> [poi()].
type_application(Tree) ->
  Type = erl_syntax:type(Tree),
  case erl_syntax_lib:analyze_type_application(Tree) of
    {Module, {Name, Arity}} ->
      %% remote type
      Id = {Module, Name, Arity},
      Pos = erl_syntax:get_pos(erl_syntax:type_application_name(Tree)),
      [poi(Pos, type_application, Id)];
    {Name, Arity} when Type =:= user_type_application ->
      %% user-defined local type
      Id = {Name, Arity},
      Pos = erl_syntax:get_pos(erl_syntax:user_type_application_name(Tree)),
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
      Args = erl_syntax:application_arguments(Tree),
      macro_name(Operator, Args);
    variable ->
      erl_syntax:variable_name(Tree);
    atom ->
      erl_syntax:atom_value(Tree);
    underscore ->
      '_'
  end.

-spec define_args(tree()) -> none | [{integer(), string()}].
define_args(Define) ->
  case erl_syntax:type(Define) of
    application ->
      Args = erl_syntax:application_arguments(Define),
      args_from_subtrees(Args);
    _ ->
      none
  end.

-spec node_name(tree()) -> atom().
node_name(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      erl_syntax:atom_value(Tree);
    variable ->
      erl_syntax:variable_name(Tree);
    underscore ->
      '_'
  end.

-spec macro_name(tree()) -> atom() | {atom(), non_neg_integer()}.
macro_name(Tree) ->
  macro_name(erl_syntax:macro_name(Tree), erl_syntax:macro_arguments(Tree)).

-spec macro_name(tree(), [tree()] | none) ->
  atom() | {atom(), non_neg_integer()}.
macro_name(Name, none) -> node_name(Name);
macro_name(Name, Args) -> {node_name(Name), length(Args)}.

-spec is_atom_node(tree()) -> {true, atom()} | false.
is_atom_node(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      {true, erl_syntax:atom_value(Tree)};
    _ ->
      false
  end.

-spec get_name_arity(tree()) -> {atom(), integer()} | false.
get_name_arity(Tree) ->
  case erl_syntax:type(Tree) of
    arity_qualifier ->
      A = erl_syntax:arity_qualifier_argument(Tree),
      case erl_syntax:type(A) of
        integer ->
          F = erl_syntax:arity_qualifier_body(Tree),
          case is_atom_node(F) of
            {true, Name} ->
              Arity = erl_syntax:integer_value(A),
              {Name, Arity};
            false ->
              false
          end;
        _ ->
          false
      end;
    _ ->
      false
  end.

-spec poi(pos() | {pos(), pos()} | erl_anno:anno(), poi_kind(), any()) -> poi().
poi(Pos, Kind, Id) ->
  poi(Pos, Kind, Id, undefined).

-spec poi(pos() | {pos(), pos()} | erl_anno:anno(), poi_kind(), any(), any()) ->
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
    Gs -> F(Tree, fold1(F, S, Gs))
  end.

-spec fold1(fun((tree(), term()) -> term()), term(), [[tree()]]) ->
  term().
fold1(F, S, [L | Ls]) ->
  fold1(F, fold2(F, S, L), Ls);
fold1(_, S, []) ->
  S.

-spec fold2(fun((tree(), term()) -> term()), term(), [tree()]) ->
  term().
fold2(F, S, [T | Ts]) ->
  fold2(F, fold(F, S, T), Ts);
fold2(_, S, []) ->
  S.

-spec subtrees(tree(), atom()) -> [[tree()]].
subtrees(Tree, application) ->
  [ case application_mfa(Tree) of
      undefined ->
        [erl_syntax:application_operator(Tree)];
      _ ->
        []
    end
  , erl_syntax:application_arguments(Tree)];
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
  , skip_function_entries(Imports) ];
attribute_subtrees(AttrName, [Exports])
  when AttrName =:= export;
       AttrName =:= export_type ->
  [ skip_function_entries(Exports) ];
attribute_subtrees(define, [_Name | Definition]) ->
  %% The definition can contain commas, in which case it will look like as if
  %% the attribute would have more than two arguments. Eg.: `-define(M, a, b).'
  [Definition];
attribute_subtrees(AttrName, _)
  when AttrName =:= include;
       AttrName =:= include_lib ->
  [];
attribute_subtrees(AttrName, [ArgTuple])
  when AttrName =:= callback;
       AttrName =:= spec ->
  case erl_syntax:type(ArgTuple) of
    tuple ->
      [FATree | Rest] = erl_syntax:tuple_elements(ArgTuple),
      [ case spec_function_name(FATree) of
          {_, _} -> [];
          undefined -> [FATree]
        end
      , Rest ];
    _ ->
      [[ArgTuple]]
  end;
attribute_subtrees(AttrName, [ArgTuple])
  when AttrName =:= type;
       AttrName =:= opaque ->
  case erl_syntax:type(ArgTuple) of
    tuple ->
      [Type | Rest] = erl_syntax:tuple_elements(ArgTuple),
      [skip_record_field_atom(Type), Rest];
    _ ->
      [ArgTuple]
  end;
attribute_subtrees(AttrName, Args)
  when is_atom(AttrName) ->
      [Args];
attribute_subtrees(AttrName, Args) ->
  %% Attribute name not an atom, probably a macro
  [[AttrName], Args].

%% Skip visiting atoms of import/export entries
-spec skip_function_entries(tree()) -> [tree()].
skip_function_entries(FunList) ->
  case erl_syntax:type(FunList) of
    list ->
      lists:filter(
        fun(FATree) ->
            case get_name_arity(FATree) of
              {_, _} -> false;
              false -> true
            end
        end, erl_syntax:list_elements(FunList));
    _ ->
      [FunList]
  end.

%% Skip visiting atoms of record and record field names as they are already
%% represented as `record_expr' or `record_field' pois
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
  PrettyClause = io_lib:format( "(~ts) ~ts"
                              , [ string:join(PrettyPatterns, ", ")
                                , PrettyGuard
                                ]),
  els_utils:to_binary(PrettyClause).

-spec record_access_location(tree()) -> erl_anno:anno().
record_access_location(Tree) ->
  %% erlfmt_parser sets start at the start of the argument expression
  %% we don't have an exact location of '#'
  %% best approximation is the end of the argument
  Start = get_end_location(erl_syntax:record_access_argument(Tree)),
  Anno = erl_syntax:get_pos(erl_syntax:record_access_type(Tree)),
  erl_anno:set_location(Start, Anno).

-spec record_expr_location(tree(), tree()) -> erl_anno:anno().
record_expr_location(Tree, RecordName) ->
  %% set start location at '#'
  %% and end location at the end of record name
  Start = record_expr_start_location(Tree),
  Anno = erl_syntax:get_pos(RecordName),
  erl_anno:set_location(Start, Anno).

-spec record_expr_start_location(tree()) -> erl_anno:location().
record_expr_start_location(Tree) ->
  %% If this is a new record creation or record type
  %% the tree start location is at '#'.
  %% However if this is a record update, then
  %% we don't have an exact location of '#',
  %% best approximation is the end of the argument.
  case erl_syntax:type(Tree) of
    record_expr ->
      case erl_syntax:record_expr_argument(Tree) of
        none ->
          get_start_location(Tree);
        RecordArg ->
          get_end_location(RecordArg)
      end;
    record_type ->
      get_start_location(Tree)
  end.

-spec macro_location(tree()) -> erl_anno:anno().
macro_location(Tree) ->
  %% set start location at '?'
  %% and end location at the end of macro name
  %% (exclude arguments)
  Start = get_start_location(Tree),
  MacroName = erl_syntax:macro_name(Tree),
  Anno = erl_syntax:get_pos(MacroName),
  erl_anno:set_location(Start, Anno).

-spec get_start_location(tree()) -> erl_anno:location().
get_start_location(Tree) ->
  erl_anno:location(erl_syntax:get_pos(Tree)).

-spec get_end_location(tree()) -> erl_anno:location().
get_end_location(Tree) ->
  %% erl_anno:end_location(erl_syntax:get_pos(Tree)).
  Anno = erl_syntax:get_pos(Tree),
  proplists:get_value(end_location, erl_anno:to_term(Anno)).
