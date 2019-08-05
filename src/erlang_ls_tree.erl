%%==============================================================================
%% Library to handle syntax trees annotated with points of interest
%%==============================================================================
-module(erlang_ls_tree).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ annotate/1
        , annotate_node/1
        , postorder_update/2
        , points_of_interest/1
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type tree() :: erl_syntax:syntaxTree().

-export_type([ tree/0 ]).

%%==============================================================================
%% API
%%==============================================================================
%% @edoc Given a syntax tree, it returns a new one, annotated with all
%% the identified _points of interest_ (a.k.a. _poi_).
-spec annotate(tree()) -> tree().
annotate(Tree) ->
  postorder_update(fun annotate_node/1, Tree).

%% @edoc Add an annotation to the root of the given `Tree` for each
%% point of interest found.
-spec annotate_node(tree()) -> tree().
annotate_node(Tree) ->
  lists:foldl(fun erl_syntax:add_ann/2, Tree, points_of_interest(Tree)).

%% @edoc Traverse the given `Tree`, applying the function `F` to all
%% nodes in the tree, in post-order. Extracted from the `erl_syntax`
%% documentation.
-spec postorder_update(fun(), tree()) -> tree().
postorder_update(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> erl_syntax:update_tree(Tree,
                                     [[postorder_update(F, Subtree)
                                       || Subtree <- Group]
                                      || Group <- List])
    end).

%% @edoc Return the list of points of interest for a given `Tree`.
-spec points_of_interest(tree()) -> [erlang_ls_poi:poi()].
points_of_interest(Tree) ->
  Type = erl_syntax:type(Tree),
  try points_of_interest(Tree, Type)
  catch
    Class:Reason ->
      lager:warning("Could not analyze tree: ~p:~p", [Class, Reason]),
      []
  end.

%% @edoc Return the list of points of interest of a specific `Type`
%% for a given `Tree`.
-spec points_of_interest(tree(), any()) -> [erlang_ls_poi:poi()].
points_of_interest(Tree, application) ->
  case erl_syntax_lib:analyze_application(Tree) of
    {M, {F, A}} ->
      %% Remote call
      [erlang_ls_poi:poi(Tree, {application, {M, F, A}})];
    {F, A} ->
      case lists:member({F, A}, erlang:module_info(exports)) of
        true ->
          %% Call to a function from the `erlang` module
          [erlang_ls_poi:poi(Tree, {application, {erlang, F, A}})];
        false ->
          %% Local call
          [erlang_ls_poi:poi(Tree, {application, {F, A}})]
      end;
    A when is_integer(A) ->
      %% If the function is not explicitly named (e.g. a variable is
      %% used as the module qualifier or the function name), only the
      %% arity A is returned.
      %% In the special case where the macro `?MODULE` is used as the
      %% module qualifier, we can consider it as a local call.
      Operator = erl_syntax:application_operator(Tree),
      try { erl_syntax:variable_name(
              erl_syntax:macro_name(
                erl_syntax:module_qualifier_argument(Operator)))
          , erl_syntax:atom_value(
              erl_syntax:module_qualifier_body(Operator))
          } of
          {'MODULE', F} ->
          [erlang_ls_poi:poi(Tree, {application, {'MODULE', F, A}})]
      catch _:_ ->
          []
      end
  end;
points_of_interest(Tree, attribute) ->
  case erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [erlang_ls_poi:poi(Tree, {behaviour, Behaviour})];
    {behaviour, {behaviour, Behaviour}} ->
      [erlang_ls_poi:poi(Tree, {behaviour, Behaviour})];
    {export, Exports} ->
      [erlang_ls_poi:poi(Tree, {exports_entry, {F, A}}) || {F, A} <- Exports];
    {module, {Module, _Args}} ->
      [erlang_ls_poi:poi(Tree, {module, Module})];
    {module, Module} ->
      [erlang_ls_poi:poi(Tree, {module, Module})];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [erlang_ls_poi:poi( Tree
                            , {define, erl_syntax:variable_name(Define)})];
        {include, [String]} ->
          [erlang_ls_poi:poi( Tree
                            , {include, erl_syntax:string_literal(String)})];
        {include_lib, [String]} ->
          [erlang_ls_poi:poi( Tree
                            , {include_lib, erl_syntax:string_literal(String)})];
        _ ->
          []
      end;
    {record, {Record, _Fields}} ->
      [erlang_ls_poi:poi(Tree, {record, atom_to_list(Record)})];
    {spec, Spec} ->
      [erlang_ls_poi:poi(Tree, {spec, Spec})];
    _ ->
      []
  end;
points_of_interest(Tree, function) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  [erlang_ls_poi:poi(Tree, {function, {F, A}})];
points_of_interest(Tree, macro) ->
  Macro = erl_syntax:variable_name(erl_syntax:macro_name(Tree)),
  [erlang_ls_poi:poi(Tree, {macro, Macro})];
points_of_interest(Tree, record_expr) ->
  Record = erl_syntax:atom_name(erl_syntax:record_expr_type(Tree)),
  [erlang_ls_poi:poi(Tree, {record_expr, Record})];
points_of_interest(_Tree, _) ->
  [].
