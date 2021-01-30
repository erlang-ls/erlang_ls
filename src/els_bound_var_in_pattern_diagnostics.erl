%%==============================================================================
%% Diagnostics detecting already bound variables in patterns
%%==============================================================================
-module(els_bound_var_in_pattern_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-behaviour(els_diagnostics).

-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      BoundVarsInPatterns = find_vars(Uri),
      [make_diagnostic(POI) || POI <- BoundVarsInPatterns];
    _ ->
      []
  end.

-spec source() -> binary().
source() ->
  <<"BoundVarInPattern">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec find_vars(uri()) -> [poi()].
find_vars(Uri) ->
  {ok, #{text := Text}} = els_utils:lookup_document(Uri),
  {ok, Forms} = parse_file(Text),
  lists:flatmap(fun find_vars_in_form/1, Forms).

-spec find_vars_in_form(erl_syntax:forms()) -> [poi()].
find_vars_in_form(Form) ->
  case erl_syntax:type(Form) of
    function ->
      AnnotatedForm = erl_syntax_lib:annotate_bindings(Form, []),
      %% There are no bound variables in function heads or guards
      %% so lets decend straight into the bodies
      Clauses = erl_syntax:function_clauses(AnnotatedForm),
      ClauseBodies = lists:map(fun erl_syntax:clause_body/1, Clauses),
      fold_subtrees(ClauseBodies, []);
    _ ->
      []
  end.

-spec fold_subtrees([[tree()]], [poi()]) -> [poi()].
fold_subtrees(Subtrees, Acc) ->
  erl_syntax_lib:foldl_listlist(fun find_vars_in_tree/2, Acc, Subtrees).

-spec find_vars_in_tree(tree(), [poi()]) -> [poi()].
find_vars_in_tree(Tree, Acc) ->
  case erl_syntax:type(Tree) of
    match_expr ->
      Pattern = erl_syntax:match_expr_pattern(Tree),
      NewAcc = fold_pattern(Pattern, Acc),
      find_vars_in_tree(erl_syntax:match_expr_body(Tree), NewAcc);
    clause ->
      Patterns = erl_syntax:clause_patterns(Tree),
      NewAcc = fold_pattern_list(Patterns, Acc),
      fold_subtrees([erl_syntax:clause_body(Tree)], NewAcc);
    _ ->
      fold_subtrees(erl_syntax:subtrees(Tree), Acc)
  end.

-spec fold_pattern(tree(), [poi()]) -> [poi()].
fold_pattern(Pattern, Acc) ->
  erl_syntax_lib:fold(fun find_vars_in_pattern/2, Acc, Pattern).

-spec fold_pattern_list([tree()], [poi()]) -> [poi()].
fold_pattern_list(Patterns, Acc) ->
  lists:foldl(fun fold_pattern/2, Acc, Patterns).

-spec find_vars_in_pattern(tree(), [poi()]) -> [poi()].
find_vars_in_pattern(Tree, Acc) ->
  case erl_syntax:type(Tree) of
    variable ->
      Var = erl_syntax:variable_name(Tree),
      Anno = erl_syntax:get_ann(Tree),
      case lists:keyfind(free, 1, Anno) of
        {free, Free} when Free =:= [Var] ->
          %% Using already bound variable in pattern
          [variable(Tree) | Acc];
        _ ->
          Acc
      end;
    _ ->
      Acc
  end.

-spec parse_file(binary()) -> {ok, erl_syntax:forms()}.
parse_file(Text) ->
  IoDevice = els_io_string:new(Text),
  {ok, Forms} = els_dodger:parse(IoDevice, {1, 1}),
  ok = file:close(IoDevice),
  {ok, Forms}.

-spec variable(tree()) -> poi().
variable(Tree) ->
  Id = erl_syntax:variable_name(Tree),
  Pos = erl_syntax:get_pos(Tree),
  Range = els_range:range(Pos, variable, Id, undefined),
  els_poi:new(Range, variable, Id, undefined).

-spec make_diagnostic(poi()) -> els_diagnostics:diagnostic().
make_diagnostic(#{id := Id, range := POIRange}) ->
  Range = els_protocol:range(POIRange),
  VariableName = atom_to_binary(Id, utf8),
  Message = <<"Bound variable in pattern: ", VariableName/binary>>,
  Severity = ?DIAGNOSTIC_HINT,
  Source = source(),
  els_diagnostics:make_diagnostic(Range, Message, Severity, Source).
