-module(erlang_ls_navigation).

-export([find_by_pos/2]).

-include("erlang_ls.hrl").

-spec find_by_pos( pos()
                 , erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()]
                 ) -> any().
find_by_pos(Pos, Tree) ->
  AnnotatedTree = postorder_update(fun annotate_with_range/1, erl_syntax:form_list(Tree)),
  {_Pos, Found} = erl_syntax_lib:fold(fun do_find_by_pos/2, {Pos, undefined}, AnnotatedTree),
  Found.

-spec do_find_by_pos(erl_syntax:syntaxTree(), {integer(), boolean()}) ->
   {integer(), boolean()}.
do_find_by_pos(Tree, {Pos, Found}) ->
  case Found of
    undefined ->
      case in_range(Pos, Tree) of
        true  -> {Pos, Tree};
        false -> {Pos, Found}
      end;
    _ ->
      {Pos, Found}
  end.

-spec in_range(integer(), erl_syntax:syntaxTree()) -> boolean().
in_range(Pos, Tree) ->
  Ann = erl_syntax:get_ann(Tree),
  case lists:keyfind(range, 1, Ann) of
    false ->
      false;
    {range, Start, End} ->
      (Start =< Pos) andalso (Pos =< End)
  end.

-spec annotate_with_range(erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
annotate_with_range(Node) ->
  case erl_syntax:type(Node) of
    application ->
      Op = erl_syntax:application_operator(Node),
      case erl_syntax:type(Op) of
        module_qualifier ->
          M = erl_syntax:module_qualifier_argument(Op),
          F = erl_syntax:module_qualifier_body(Op),
          Start = erl_syntax:get_pos(M),
          {Line, Column} = erl_syntax:get_pos(F),
          End = {Line, Column + length(erl_syntax:atom_name(F))},
          erl_syntax:add_ann({range, Start, End}, Node);
        _ ->
          Node
      end;
    _ ->
      Node
  end.

-spec postorder_update(fun(), erl_syntax:syntaxTree()) ->
   erl_syntax:syntaxTree().
postorder_update(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> erl_syntax:update_tree(Tree,
                                     [[postorder_update(F, Subtree)
                                       || Subtree <- Group]
                                      || Group <- List])
    end).
