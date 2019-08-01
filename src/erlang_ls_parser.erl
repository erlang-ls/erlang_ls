-module(erlang_ls_parser).

-export([ find_poi_by_info/2
        , find_poi_by_info_key/2
        , find_poi_by_pos/2
        , list_poi/1
        , parse/1
        , parse_file/1
        ]).

-type line()        :: non_neg_integer().
-type column()      :: non_neg_integer().
-type pos()         :: {line(), column()}.
-type range()       :: #{ from := pos(), to := pos() }.
%% Point of Interest
-type poi()         :: #{ type := atom(), info => any(), range := range()}.

-export_type([ poi/0
             , range/0
             ]).

%% TODO: Generate random filename
%% TODO: Ideally avoid writing to file at all (require epp changes)
-define(TMP_PATH, <<"/tmp/erlang_ls_tmp">>).

-spec parse(binary()) -> {ok, syntax_tree()}.
parse(Text) ->
  %% epp_dodger only works with source files,
  %% so let's use a temporary file.
  ok = file:write_file(?TMP_PATH, Text),
  parse_file(?TMP_PATH).

-spec parse_file(binary()) -> {ok, syntax_tree()} | {error, any()}.
parse_file(Path) ->
  case file:open(Path, [read]) of
    {ok, IoDevice} ->
      %% Providing `{1, 1}` as the initial location ensures
      %% that the returned forms include column numbers, as well.
      %% The specs for the epp_dodger API are slightly incorrect.
      %% A bug has been reported (see https://bugs.erlang.org/browse/ERL-1005)
      %% Meanwhile, let's trick Dialyzer with an apply.
      {ok, Forms} = erlang:apply(epp_dodger, parse, [IoDevice, {1, 1}]),
      Tree = erl_syntax:form_list(Forms),
      ok = file:close(IoDevice),
      {ok, Tree};
    {error, Error} ->
      {error, Error}
  end.

-spec get_range(syntax_tree(), pos(), {atom(), any()}) -> range().
get_range({Line, Column}, {application, {M, F, _A}}) ->
  CFrom = Column - length(atom_to_list(M)),
  From = {Line, CFrom},
  CTo = Column + length(atom_to_list(F)),
  To = {Line, CTo},
  #{ from => From, to => To };
get_range({Line, Column}, {application, {F, _A}}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
get_range({Line, Column}, {behaviour, Behaviour}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
get_range({_Line, _Column}, {exports_entry, {_F, _A}}) ->
  %% TODO: The location information for the arity qualifiers are lost during
  %%       parsing in `epp_dodger`. This requires fixing.
  #{ from => {0, 0}, to => {0, 0} };
get_range({Line, Column}, {function, {F, _A}}) ->
  From = {Line - 1, Column - 1},
  To = {Line - 1, Column + length(atom_to_list(F)) - 1},
  #{ from => From, to => To };
get_range({Line, _Column}, {define, _Define}) ->
  From = {Line - 1, 0},
  To = From,
  #{ from => From, to => To };
get_range({Line, Column}, {include, Include}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include") + length(Include)},
  #{ from => From, to => To };
get_range({Line, Column}, {include_lib, Include}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include_lib") + length(Include)},
  #{ from => From, to => To };
get_range({Line, Column}, {macro, Macro}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
get_range({Line, Column}, {module, _}) ->
  From = {Line - 1, Column - 1},
  To = From,
  #{ from => From, to => To };
get_range({Line, Column}, {record_expr, Record}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(Record) - 1},
  #{ from => From, to => To };
%% TODO: Distinguish between usage poi and definition poi
get_range({Line, _Column}, {record, _Record}) ->
  From = {Line - 1, 0},
  To = From,
  #{ from => From, to => To };
get_range({_Line, _Column}, {spec, _Spec}) ->
  %% TODO: The location information for the arity qualifiers are lost during
  %%       parsing in `epp_dodger`. This requires fixing.
  #{ from => {0, 0}, to => {0, 0} }.

-spec find_poi_by_info(syntax_tree(), any()) -> [poi()].
find_poi_by_info(Tree, Info0) ->
  [POI || #{info := Info} = POI <- list_poi(Tree), Info0 =:= Info].

%% TODO: Rename
-spec find_poi_by_info_key(syntax_tree(), atom()) -> [poi()].
find_poi_by_info_key(Tree, Key0) ->
  [POI || #{info := {Key, _}} = POI <- list_poi(Tree), Key0 =:= Key].

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

-spec poi(syntax_tree(), any()) -> poi().
poi(Tree, Info) ->
  Pos = erl_syntax:get_pos(Tree),
  Range = get_range(Pos, Info),
  #{ type  => poi
   , info  => Info
   , range => Range
   }.
