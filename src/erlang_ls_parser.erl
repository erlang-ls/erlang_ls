-module(erlang_ls_parser).

-export([ find_poi_by_info/2
        , find_poi_by_info_key/2
        , find_poi_by_pos/2
        , list_poi/1
        , parse/1
        , parse_file/1
        ]).

%% TODO: Generate random filename
%% TODO: Ideally avoid writing to file at all (require epp changes)
-define(TMP_PATH, <<"/tmp/erlang_ls_tmp">>).

-spec parse(binary()) -> {ok, erlang_ls_tree:tree()}.
parse(Text) ->
  %% epp_dodger only works with source files,
  %% so let's use a temporary file.
  ok = file:write_file(?TMP_PATH, Text),
  parse_file(?TMP_PATH).

-spec parse_file(binary()) ->
   {ok, erlang_ls_tree:tree()} | {error, any()}.
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

-spec find_poi_by_info(erlang_ls_tree:tree(), any()) ->
   [erlang_ls_poi:poi()].
find_poi_by_info(Tree, Info0) ->
  [POI || #{info := Info} = POI <- list_poi(Tree), Info0 =:= Info].

%% TODO: Rename
-spec find_poi_by_info_key(erlang_ls_tree:tree(), atom()) ->
   [erlang_ls_poi:poi()].
find_poi_by_info_key(Tree, Key0) ->
  [POI || #{info := {Key, _}} = POI <- list_poi(Tree), Key0 =:= Key].

-spec find_poi_by_pos(erlang_ls_tree:tree(), erlang_ls_poi:pos()) ->
   [erlang_ls_poi:poi()].
find_poi_by_pos(Tree, Pos) ->
  [POI || #{range := Range} = POI <- list_poi(Tree), matches_pos(Pos, Range)].

-spec list_poi(erlang_ls_tree:tree()) -> [erlang_ls_poi:poi()].
list_poi(Tree) ->
  F = fun(T, Acc) ->
          Annotations = erl_syntax:get_ann(T),
          case [POI || #{ type := poi } = POI <- Annotations] of
            [] -> Acc;
            L -> L ++ Acc
          end
      end,
  erl_syntax_lib:fold(F, [], Tree).

-spec matches_pos(erlang_ls_poi:pos(), erlang_ls_poi:range()) -> boolean().
matches_pos(Pos, #{from := From, to := To}) ->
  (From =< Pos) andalso (Pos =< To).
