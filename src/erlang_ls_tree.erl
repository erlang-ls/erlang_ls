%%==============================================================================
%% Library to handle syntax trees annotated with points of interest
%%==============================================================================
-module(erlang_ls_tree).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ annotate/1
        , annotate/2
        , annotate_node/1
        , annotate_node/2
        , postorder_update/3
        , points_of_interest/1
        , points_of_interest/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type tree() :: erl_syntax:syntaxTree().
-type annotated_tree() :: tree().
-type extra() :: map(). %% TODO: Refine type

-export_type([ annotated_tree/0
             , extra/0
             , tree/0
             ]).

%%==============================================================================
%% API
%%==============================================================================

%% @edoc Given a syntax tree, it returns a new one, annotated with all
%% the identified _points of interest_ (a.k.a. _poi_).
-spec annotate(tree()) -> tree().
annotate(Tree) ->
  annotate(Tree, #{}).

%% @edoc Given a syntax tree, it returns a new one, annotated with all
%% the identified _points of interest_ (a.k.a. _poi_).
-spec annotate(tree(), extra()) -> tree().
annotate(Tree, Extra) ->
  postorder_update(fun annotate_node/2, Tree, Extra).

%% @edoc Add an annotation to the root of the given `Tree` for each
%% point of interest found.
-spec annotate_node(tree()) -> tree().
annotate_node(Tree) ->
  annotate_node(Tree, #{}).

%% @edoc Add an annotation to the root of the given `Tree` for each
%% point of interest found.
-spec annotate_node(tree(), extra()) -> tree().
annotate_node(Tree, Extra) ->
  lists:foldl( fun erl_syntax:add_ann/2
             , Tree
             , points_of_interest(Tree, Extra)).

%% @edoc Traverse the given `Tree`, applying the function `F` to all
%% nodes in the tree, in post-order. Adapted from the `erl_syntax`
%% documentation.
-spec postorder_update(fun(), tree(), extra()) -> tree().
postorder_update(F, Tree, Extra) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> erl_syntax:update_tree(Tree,
                                     [[postorder_update(F, Subtree, Extra)
                                       || Subtree <- Group]
                                      || Group <- List])
    end, Extra).

%% @edoc Return the list of points of interest for a given `Tree`.
-spec points_of_interest(tree()) -> [erlang_ls_poi:poi()].
points_of_interest(Tree) ->
  points_of_interest(Tree, #{}).

%% @edoc Return the list of points of interest for a given `Tree`.
-spec points_of_interest(tree(), extra()) -> [erlang_ls_poi:poi()].
points_of_interest(Tree, Extra) ->
  try
    case erl_syntax:type(Tree) of
      application   -> application(Tree, Extra);
      attribute     -> attribute(Tree, Extra);
      function      -> function(Tree, Extra);
      implicit_fun  -> implicit_fun(Tree, Extra);
      macro         -> macro(Tree, Extra);
      record_access -> record_access(Tree, Extra);
      record_expr   -> record_expr(Tree, Extra);
      _             -> []
    end
  catch
    Class:Reason:Stacktrace ->
      Position = erl_syntax:get_pos(Tree),
      lager:warning( "Could not analyze tree - error=~p:~p pos=~p "
                     "stacktrace=~p"
                   , [Class, Reason, Position, Stacktrace]),
      []
  end.

-spec application(tree(), extra()) -> [erlang_ls_poi:poi()].
application(Tree, Extra) ->
  case application_mfa(Tree) of
    undefined -> [];
    MFA -> [erlang_ls_poi:poi(Tree, application, MFA, Extra)]
  end.

-spec application_mfa(tree()) ->
  {module(), atom(), arity()} | {atom(), arity()} | undefined.
application_mfa(Tree) ->
  case erl_syntax_lib:analyze_application(Tree) of
    %% Remote call
    {M, {F, A}} -> {M, F, A};
    {F, A} ->
      case lists:member({F, A}, erlang:module_info(exports)) of
        %% Call to a function from the `erlang` module
              true -> {erlang, F, A};
        %% Local call
        false -> {F, A}
      end;
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

-spec attribute(tree(), extra()) -> [erlang_ls_poi:poi()].
attribute(Tree, Extra) ->
  try erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [erlang_ls_poi:poi(Tree, behaviour, Behaviour, Extra)];
    {behaviour, {behaviour, Behaviour}} ->
      [erlang_ls_poi:poi(Tree, behaviour, Behaviour, Extra)];
    {export, Exports} ->
      [erlang_ls_poi:poi(Tree, exports_entry, {F, A}, Extra) || {F, A} <- Exports];
    {import, {M, Imports}} ->
      [erlang_ls_poi:poi(Tree, import_entry, {M, F, A}, Extra) || {F, A} <- Imports];
    {module, {Module, _Args}} ->
      [erlang_ls_poi:poi(Tree, module, Module, Extra)];
    {module, Module} ->
      [erlang_ls_poi:poi(Tree, module, Module, Extra)];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [erlang_ls_poi:poi( Tree
                            , define
                            , define_name(Define)
                            , Extra )];
        {include, [String]} ->
          [erlang_ls_poi:poi( Tree
                            , include
                            , erl_syntax:string_literal(String)
                            , Extra )];
        {include_lib, [String]} ->
          [erlang_ls_poi:poi( Tree
                            , include_lib
                            , erl_syntax:string_literal(String)
                            , Extra )];
        _ ->
          []
      end;
    {record, {Record, _Fields}} ->
      [erlang_ls_poi:poi(Tree, record, atom_to_list(Record), Extra)];
    {spec, {spec, {{F, A}, _}}} ->
      SpecLocations = maps:get(spec_locations, Extra, []),
      Locations     = proplists:get_value({F, A}, SpecLocations),
      [erlang_ls_poi:poi(Tree, type_application, {T, L}, Extra) || {T, L} <- Locations];
    {type, {type, Type}} ->
      %% TODO: Support type usages in type definitions
      TypeName = element(1, Type),
      [erlang_ls_poi:poi(Tree, type_definition, TypeName, Extra)];
    _ ->
      []
  catch throw:syntax_error ->
      []
  end.

-spec function(tree(), extra()) -> [erlang_ls_poi:poi()].
function(Tree, Extra) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  [erlang_ls_poi:poi(Tree, function, {F, A}, Extra)].

-spec implicit_fun(tree(), extra()) -> [erlang_ls_poi:poi()].
implicit_fun(Tree, Extra) ->
  FunSpec = try erl_syntax_lib:analyze_implicit_fun(Tree) of
              {M, {F, A}} -> {M, F, A};
              {F, A} -> {F, A}
            catch throw:syntax_error ->
                undefined
            end,
  case FunSpec of
    undefined -> [];
    _ -> [erlang_ls_poi:poi(Tree, implicit_fun, FunSpec, Extra)]
  end.

-spec macro(tree(), extra()) -> [erlang_ls_poi:poi()].
macro(Tree, Extra) ->
  case erl_syntax:get_pos(Tree) of
    0 -> [];
    _ -> [erlang_ls_poi:poi(Tree, macro, node_name(Tree), Extra)]
  end.

-spec record_access(tree(), extra()) -> [erlang_ls_poi:poi()].
record_access(Tree, Extra) ->
  RecordNode = erl_syntax:record_access_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_name(RecordNode),
      Field = erl_syntax:atom_name(erl_syntax:record_access_field(Tree)),
      [erlang_ls_poi:poi(Tree, record_access, {Record, Field}, Extra)];
    _ ->
      []
  end.

-spec record_expr(tree(), extra()) -> [erlang_ls_poi:poi()].
record_expr(Tree, Extra) ->
  RecordNode = erl_syntax:record_expr_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_name(RecordNode),
      [erlang_ls_poi:poi(Tree, record_expr, Record, Extra)];
    _ ->
      []
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
      erl_syntax:atom_value(Tree)
  end.

-spec node_name(tree()) -> atom().
node_name(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      erl_syntax:atom_value(Tree);
    variable ->
      erl_syntax:variable_name(Tree);
    macro ->
      node_name(erl_syntax:macro_name(Tree))
  end.
