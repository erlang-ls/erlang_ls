%% TODO: Rename into erlang_ls_syntax
-module(erlang_ls_parser).

-export([ annotate/1
        , find_by_pos/2
        , parse/1
        , parse_file/1
        ]).

-export([ postorder_update/2
        ]).

-type syntax_tree() :: erl_syntax:syntaxTree().
-type line()        :: non_neg_integer().
-type column()      :: non_neg_integer().
-type pos()         :: {line(), column()}.
-type range()       :: #{ from := pos(), to := pos() }.
%% Point of Interest
-type poi()         :: #{ type := atom(), info => any(), range := range()}.

%% TODO: Generate random filename
%% TODO: Ideally avoid writing to file at all (require epp changes)
-define(TMP_PATH, "/tmp/erlang_ls_tmp").

-spec parse(binary()) -> {ok, syntax_tree()}.
parse(Text) ->
  %% epp_dodger only works with source files,
  %% so let's use a temporary file.
  ok = file:write_file(?TMP_PATH, Text),
  parse_file(?TMP_PATH).

-spec parse_file(string()) -> {ok, syntax_tree()} | {error, any()}.
parse_file(Path) ->
  case file:open(Path, [read]) of
    {ok, IoDevice} ->
      %% Providing `{1, 1}` as the second argument ensures
      %% that the returned forms include column numbers, as well.
      {ok, Forms} = epp_dodger:parse(IoDevice, {1, 1}),
      Tree = erl_syntax:form_list(Forms),
      ok = file:close(IoDevice),
      {ok, Tree};
    {error, Error} ->
      {error, Error}
  end.

%% Create annotations for the points of interest (aka `poi`) in the
%% tree.
-spec annotate(syntax_tree()) -> syntax_tree().
annotate(Tree) ->
  case analyze(Tree) of
    undefined -> Tree;
    Poi -> erl_syntax:add_ann(Poi, Tree)
  end.

%% Extracted from the `erl_syntax` documentation.
-spec postorder_update(fun(), syntax_tree()) -> syntax_tree().
postorder_update(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> erl_syntax:update_tree(Tree,
                                     [[postorder_update(F, Subtree)
                                       || Subtree <- Group]
                                      || Group <- List])
    end).

-spec get_range(syntax_tree(), {atom(), any()}) -> range().
get_range(Tree, {behaviour, Behaviour}) ->
  {Line, Column} = erl_syntax:get_pos(Tree),
  From = {Line, Column - 1},
  To = {Line, length("behaviour") + length(atom_to_list(Behaviour)) + 1},
  #{ from => From, to => To }.

-spec find_by_pos(syntax_tree(), pos()) -> [poi()].
find_by_pos(Tree, Pos) ->
  F = fun(T, Acc) ->
          Annotations = erl_syntax:get_ann(T),
          case [Info || #{ type  := poi
                         , info  := Info
                         , range := Range }
                          <- Annotations, matches_pos(Pos, Range)] of
            [] -> Acc;
            L -> L ++ Acc
          end
      end,
  erl_syntax_lib:fold(F, [], Tree).

-spec matches_pos(pos(), range()) -> boolean().
matches_pos(Pos, #{from := From, to := To}) ->
  (From =< Pos) andalso (Pos =< To).

-spec analyze(syntax_tree()) -> poi() | undefined.
analyze(Tree) ->
  Type = erl_syntax:type(Tree),
  analyze(Tree, Type).

-spec analyze(syntax_tree(), any()) -> poi() | undefined.
analyze(Tree, attribute) ->
  case erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      poi(Tree, {behaviour, Behaviour});
    {behaviour, {behaviour, Behaviour}} ->
      poi(Tree, {behaviour, Behaviour});
    _ ->
      undefined
  end;
analyze(_Tree, _) ->
  undefined.

-spec poi(syntax_tree(), any()) -> poi().
poi(Tree, Info) ->
  Range = get_range(Tree, Info),
  #{ type  => poi
   , info  => Info
   , range => Range
   }.
