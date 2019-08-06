%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(erlang_ls_code_navigation).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ goto_definition/2
        , goto_definition/3
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(OTP_INCLUDE_PATH, "/usr/local/Cellar/erlang/21.2.4/lib/erlang/lib").
%% TODO: Implement support for workspaces
-define(ERLANG_LS_PATH, "/Users/robert.aloi/git/github/erlang-ls/erlang_ls").
-define(TEST_APP_PATH, "/Users/robert.aloi/git/github/erlang-ls/test").
-define(DEPS_PATH, "/Users/robert.aloi/git/github/erlang-ls/erlang_ls/_build/debug/lib").

%%==============================================================================
%% API
%%==============================================================================

-spec goto_definition(binary(), erlang_ls_poi:poi()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_definition(Filename, POI) ->
  goto_definition(Filename, POI, full_path()).

-spec goto_definition(binary(), erlang_ls_poi:poi(), [string()]) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_definition( _Filename
               , #{ info := {application, {M, _F, _A}} = Info }
               , Path) ->
  case erlang_ls_tree:annotate_file(filename(M), Path) of
    {ok, FullName, AnnotatedTree} ->
      case erlang_ls_poi:match(AnnotatedTree, definition(Info)) of
        [#{ range := Range }] ->
          {ok, FullName, Range};
        [] ->
          {error, not_found}
      end;
    {error, Error} ->
      {error, Error}
  end;
goto_definition( Filename
               , #{ info := {application, {_F, _A}} = Info }
               , Path) ->
  case erlang_ls_tree:annotate_file(filename:basename(Filename), Path) of
    {ok, FullName, AnnotatedTree} ->
      case erlang_ls_poi:match(AnnotatedTree, definition(Info)) of
        [#{ range := Range }] ->
          {ok, FullName, Range};
        [] ->
          {error, not_found}
      end;
    {error, Error} ->
      {error, Error}
  end;
goto_definition(_Filename, #{ info := {behaviour, Behaviour} = Info }, Path) ->
  search(filename(Behaviour), Path, definition(Info));
%% TODO: Eventually search everywhere and suggest a code lens to include a file
goto_definition(Filename, #{ info := {macro, _Define} = Info }, Path) ->
  search(filename:basename(Filename), Path, definition(Info));
goto_definition(Filename, #{ info := {record_expr, _Record} = Info }, Path) ->
  search(filename:basename(Filename), Path, definition(Info));
goto_definition(_Filename, #{ info := {include, Include0} }, Path) ->
  Include = list_to_binary(string:trim(Include0, both, [$"])),
  case erlang_ls_tree:annotate_file(Include, Path) of
    {ok, FullName, _AnnotatedTree} ->
      {ok, FullName, #{ from => {0, 0}, to => {0, 0} }};
    {error, Error} ->
      {error, Error}
  end;
goto_definition(_Filename, #{ info := {include_lib, Include0} }, Path) ->
  Include = list_to_binary(lists:last(filename:split(string:trim(Include0, both, [$"])))),
  case erlang_ls_tree:annotate_file(Include, Path) of
    {ok, FullName, _AnnotatedTree} ->
      {ok, FullName, #{ from => {0, 0}, to => {0, 0} }};
    {error, Error} ->
      {error, Error}
  end;
goto_definition(_Filename, _, _Path) ->
  {error, not_found}.

-spec definition({atom(), any()}) -> {atom(), any()}.
definition({application, {_M, F, A}}) ->
  {function, {F, A}};
definition({application, {F, A}}) ->
  {function, {F, A}};
definition({behaviour, Behaviour}) ->
  {module, Behaviour};
definition({macro, Define}) ->
  {define, Define};
definition({record_expr, Record}) ->
  {record, Record}.

-spec otp_path() -> [string()].
otp_path() ->
  filelib:wildcard(filename:join([?OTP_INCLUDE_PATH, "*/src"])).

-spec app_path() -> [string()].
app_path() ->
  [ filename:join([?TEST_APP_PATH, "src"])
  , filename:join([?TEST_APP_PATH, "include"])
  , filename:join([?ERLANG_LS_PATH, "src"])
  , filename:join([?TEST_APP_PATH, "include"])
  ].

-spec deps_path() -> [string()].
deps_path() ->
  filelib:wildcard(filename:join([?DEPS_PATH, "*/src"])).

full_path() ->
  lists:append( [ app_path() , deps_path() , otp_path() ]).

%% Look for a definition recursively in a file and its includes.
-spec search(binary(), [string()], any()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
search(Filename, Path, Thing) ->
  case erlang_ls_tree:annotate_file(Filename, Path) of
    {ok, FullName, AnnotatedTree} ->
      case find(AnnotatedTree, Thing) of
        {error, not_found} ->
          Includes = erlang_ls_poi:match_key(AnnotatedTree, include),
          IncludeLibs = erlang_ls_poi:match_key(AnnotatedTree, include_lib),
          search_in_includes(Includes ++ IncludeLibs, Thing);
        {ok, Range} ->
          {ok, FullName, Range}
      end;
    {error, Error} ->
      {error, Error}
  end.

%% Look for a definition in a given tree
-spec find(erlang_ls_tree:tree(), any()) ->
   {ok, erlang_ls_poi:range()} | {error, any()}.
find(AnnotatedTree, Thing) ->
  case erlang_ls_poi:match(AnnotatedTree, Thing) of
    [#{ range := Range }|_] ->
      {ok, Range};
    [] ->
      {error, not_found}
  end.

-spec search_in_includes([erlang_ls_poi:poi()], string()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
search_in_includes([], _Thing) ->
  {error, not_found};
search_in_includes([#{info := Info}|T], Thing) ->
  Include = normalize_include(Info),
  case search(list_to_binary(Include), app_path(), Thing) of
    {error, not_found} -> search_in_includes(T, Thing);
    {ok, FullName, Range} -> {ok, FullName, Range}
  end.

-spec normalize_include({atom(), string()}) -> string().
normalize_include({include, Include}) ->
  string:trim(Include, both, [$"]);
normalize_include({include_lib, Include}) ->
  lists:last(filename:split(string:trim(Include, both, [$"]))).

-spec filename(atom()) -> binary().
filename(Module) ->
  list_to_binary(atom_to_list(Module) ++ ".erl").
