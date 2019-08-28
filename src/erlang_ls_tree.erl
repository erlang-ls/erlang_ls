%%==============================================================================
%% Library to handle syntax trees annotated with points of interest
%%==============================================================================
-module(erlang_ls_tree).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ annotate/1
        , annotate/2
        , annotate_file/2
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

-spec annotate_file(binary(), [string()]) ->
   {ok, binary(), annotated_tree()} | {error, any()}.
annotate_file(Filename, Path) ->
  case file:path_open(Path, Filename, [read]) of
    {ok, IoDevice, FullName} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      {ok, Tree, Extra} = erlang_ls_parser:parse_file(FullName),
      {ok, FullName, annotate(Tree, Extra)};
    {error, Error} ->
      {error, Error}
  end.

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
  Type = erl_syntax:type(Tree),
  try points_of_interest(Tree, Type, Extra)
  catch
    Class:Reason ->
      lager:warning("Could not analyze tree: ~p:~p", [Class, Reason]),
      []
  end.

%% @edoc Return the list of points of interest of a specific `Type`
%% for a given `Tree`.
-spec points_of_interest(tree(), any(), [[erlang_ls_poi:pos()]]) -> [erlang_ls_poi:poi()].
points_of_interest(Tree, application, Extra) ->
  case erl_syntax_lib:analyze_application(Tree) of
    {M, {F, A}} ->
      %% Remote call
      [erlang_ls_poi:poi(Tree, {application, {M, F, A}}, Extra)];
    {F, A} ->
      case lists:member({F, A}, erlang:module_info(exports)) of
        true ->
          %% Call to a function from the `erlang` module
          [erlang_ls_poi:poi(Tree, {application, {erlang, F, A}}, Extra)];
        false ->
          %% Local call
          [erlang_ls_poi:poi(Tree, {application, {F, A}}, Extra)]
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
          [erlang_ls_poi:poi(Tree, {application, {'MODULE', F, A}}, Extra)]
      catch _:_ ->
          []
      end
  end;
points_of_interest(Tree, attribute, Extra) ->
  case erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [erlang_ls_poi:poi(Tree, {behaviour, Behaviour}, Extra)];
    {behaviour, {behaviour, Behaviour}} ->
      [erlang_ls_poi:poi(Tree, {behaviour, Behaviour}, Extra)];
    {export, Exports} ->
      [erlang_ls_poi:poi(Tree, {exports_entry, {F, A}}, Extra) || {F, A} <- Exports];
    {import, {M, Imports}} ->
      [erlang_ls_poi:poi(Tree, {import_entry, {M, F, A}}, Extra) || {F, A} <- Imports];
    {module, {Module, _Args}} ->
      [erlang_ls_poi:poi(Tree, {module, Module}, Extra)];
    {module, Module} ->
      [erlang_ls_poi:poi(Tree, {module, Module}, Extra)];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [erlang_ls_poi:poi( Tree
                            , {define, define_name(Define)}
                            , Extra )];
        {include, [String]} ->
          [erlang_ls_poi:poi( Tree
                            , {include, erl_syntax:string_literal(String)}
                            , Extra )];
        {include_lib, [String]} ->
          [erlang_ls_poi:poi( Tree
                            , {include_lib, erl_syntax:string_literal(String)}
                            , Extra )];
        _ ->
          []
      end;
    {record, {Record, _Fields}} ->
      [erlang_ls_poi:poi(Tree, {record, atom_to_list(Record)}, Extra)];
    {spec, {spec, {{F, A}, _}}} ->
      SpecLocations = maps:get(spec_locations, Extra, []),
      Locations = proplists:get_value({F, A}, SpecLocations),
      [erlang_ls_poi:poi(Tree, {type_application, {T, L}}, Extra) || {T, L} <- Locations];
    {type, {type, Type}} ->
      %% TODO: Support type usages in type definitions
      TypeName = element(1, Type),
      [erlang_ls_poi:poi(Tree, {type_definition, TypeName}, Extra)];
    _ ->
      []
  end;
points_of_interest(Tree, function, Extra) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  [erlang_ls_poi:poi(Tree, {function, {F, A}}, Extra)];
points_of_interest(Tree, implicit_fun, Extra) ->
  FunSpec = case erl_syntax_lib:analyze_implicit_fun(Tree) of
              {M, {F, A}} -> {M, F, A};
              {F, A} -> {F, A}
            end,
  [erlang_ls_poi:poi(Tree, {implicit_fun, FunSpec}, Extra)];
points_of_interest(Tree, macro, Extra) ->
  Name = erl_syntax:macro_name(Tree),
  [erlang_ls_poi:poi(Tree, {macro, macro_name(Name)}, Extra)];
points_of_interest(Tree, record_expr, Extra) ->
  Record = erl_syntax:atom_name(erl_syntax:record_expr_type(Tree)),
  [erlang_ls_poi:poi(Tree, {record_expr, Record}, Extra)];
points_of_interest(_Tree, _Type, _Extra) ->
  [].

-spec define_name(tree()) -> atom().
define_name(Tree) ->
  case erl_syntax:type(Tree) of
    application ->
      Operator = erl_syntax:application_operator(Tree),
      case erl_syntax:type(Operator) of
        atom     -> erl_syntax:atom_value(Operator);
        variable -> erl_syntax:variable_name(Operator)
      end;
    variable ->
      erl_syntax:variable_name(Tree)
  end.

-spec macro_name(tree()) -> string().
macro_name(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      erl_syntax:atom_name(Tree);
    variable ->
      erl_syntax:variable_name(Tree)
  end.
