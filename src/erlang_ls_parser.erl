%% TODO: Rename into erlang_ls_syntax
-module(erlang_ls_parser).

-export([ annotate/1
        , annotate_node/1
        , find_poi_by_pos/2
        , list_poi/1
        , parse/1
        , parse_file/1
        , postorder_update/2
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
      %% Providing `{1, 1}` as the initial location ensures
      %% that the returned forms include column numbers, as well.
      {ok, Forms} = epp_dodger:parse(IoDevice, {1, 1}),
      Tree = erl_syntax:form_list(Forms),
      ok = file:close(IoDevice),
      {ok, Tree};
    {error, Error} ->
      {error, Error}
  end.

-spec annotate(syntax_tree()) -> syntax_tree().
annotate(Tree) ->
  postorder_update(fun annotate_node/1, Tree).

%% Create annotations for the points of interest (aka `poi`) in the
%% tree.
-spec annotate_node(syntax_tree()) -> syntax_tree().
annotate_node(Tree) ->
  lists:foldl(fun erl_syntax:add_ann/2, Tree, analyze(Tree)).

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

-spec get_range(syntax_tree(), pos(), {atom(), any()}) -> range().
get_range(_Tree, {Line, Column}, {behaviour, Behaviour}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
get_range(_Tree, {_Line, _Column}, {exports_entry, {_F, _A}}) ->
  %% TODO: The location information for the arity qualifiers are lost during
  %%       parsing in `epp_dodger`. This requires fixing.
  #{ from => {0, 0}, to => {0, 0} };
get_range(_Tree, {Line, Column}, {include, Include}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include") + length(Include)},
  #{ from => From, to => To };
get_range(_Tree, {Line, Column}, {include_lib, Include}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include_lib") + length(Include)},
  #{ from => From, to => To };
get_range(_Tree, {Line, Column}, {macro, Macro}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
get_range(_Tree, {Line, Column}, {record_expr, Record}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(Record) - 1},
  #{ from => From, to => To };
get_range(_Tree, {_Line, _Column}, {spec, _Spec}) ->
  %% TODO: The location information for the arity qualifiers are lost during
  %%       parsing in `epp_dodger`. This requires fixing.
  #{ from => {0, 0}, to => {0, 0} }.

-spec find_poi_by_pos(syntax_tree(), pos()) -> [poi()].
find_poi_by_pos(Tree, Pos) ->
  [POI || #{range := Range} = POI <- list_poi(Tree), matches_pos(Pos, Range)].

-spec list_poi(syntax_tree()) -> [poi()].
list_poi(Tree) ->
  F = fun(T, Acc) ->
          Annotations = erl_syntax:get_ann(T),
          case [POI || #{ type := poi } = POI <- Annotations] of
            [] -> Acc;
            L -> L ++ Acc
          end
      end,
  erl_syntax_lib:fold(F, [], Tree).

-spec matches_pos(pos(), range()) -> boolean().
matches_pos(Pos, #{from := From, to := To}) ->
  (From =< Pos) andalso (Pos =< To).

-spec analyze(syntax_tree()) -> [poi()].
analyze(Tree) ->
  Type = erl_syntax:type(Tree),
  try analyze(Tree, Type)
  catch
    Class:Reason ->
      lager:warning("Could not analyze tree: ~p:~p", [Class, Reason]),
      erlang:display({Class, Reason}),
      []
  end.

-spec analyze(syntax_tree(), any()) -> [poi()].
analyze(Tree, attribute) ->
  case erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [poi(Tree, {behaviour, Behaviour})];
    {behaviour, {behaviour, Behaviour}} ->
      [poi(Tree, {behaviour, Behaviour})];
    {export, Exports} ->
      [poi(Tree, {exports_entry, {F, A}}) || {F, A} <- Exports];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {include, [String]} ->
          [poi(Tree, {include, erl_syntax:string_literal(String)})];
        {include_lib, [String]} ->
          [poi(Tree, {include_lib, erl_syntax:string_literal(String)})];
        _ ->
          []
      end;
    {spec, Spec} ->
      [poi(Tree, {spec, Spec})];
    _ ->
      []
  end;
analyze(Tree, macro) ->
  Macro = erl_syntax:variable_name(erl_syntax:macro_name(Tree)),
  [poi(Tree, {macro, Macro})];
analyze(Tree, record_expr) ->
  Record = erl_syntax:atom_name(erl_syntax:record_expr_type(Tree)),
  [poi(Tree, {record_expr, Record})];
analyze(_Tree, _) ->
  [].

-spec poi(syntax_tree(), any()) -> poi().
poi(Tree, Info) ->
  Pos = erl_syntax:get_pos(Tree),
  Range = get_range(Tree, Pos, Info),
  #{ type  => poi
   , info  => Info
   , range => Range
   }.
