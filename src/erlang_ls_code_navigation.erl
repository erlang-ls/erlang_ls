%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(erlang_ls_code_navigation).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ goto_definition/2 ]).

-export([ app_path/0
        , deps_path/0
        , otp_path/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================

-spec goto_definition(binary(), erlang_ls_poi:poi()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_definition(Filename, POI) ->
  goto_definition(Filename, POI, include_path()).

%% TODO: Abstract pattern
-spec goto_definition(binary(), erlang_ls_poi:poi(), [string()]) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_definition( _Filename
               , #{ kind := Kind, data := {M, _F, _A} = Data }
               , Path
               ) when Kind =:= application;
                      Kind =:= implicit_fun ->
  goto_fun_definition(filename(M), Path, Kind, Data);
goto_definition( Filename
               , #{ kind := Kind
                  , data := {_F, _A} = Data
                  }
               , Path
               ) when Kind =:= application;
                      Kind =:= implicit_fun;
                      Kind =:= exports_entry ->
  goto_fun_definition(filename:basename(Filename), Path, Kind, Data);
goto_definition(_Filename, #{ kind := behaviour
                            , data := Behaviour
                            }, Path) ->
  search(filename(Behaviour), Path, definition(behaviour, Behaviour));
goto_definition( _Filename
               , #{ kind := import_entry
                  , data := {M, _F, _A} = Data
                  }
               , Path) ->
  search(filename(M), Path, definition(import_entry, Data));
%% TODO: Eventually search everywhere and suggest a code lens to include a file
goto_definition(Filename, #{ kind := macro, data := Data }, Path) ->
  search(filename:basename(Filename), Path, definition(macro, Data));
goto_definition(Filename, #{ kind := record_access
                           , data := Data
                           }, Path) ->
  search(filename:basename(Filename), Path, definition(record_access, Data));
goto_definition(Filename, #{ kind := record_expr, data := Data }, Path) ->
  search(filename:basename(Filename), Path, definition(record_expr, Data));
goto_definition(_Filename, #{ kind := include, data := Include0 }, Path) ->
  Include = list_to_binary(string:trim(Include0, both, [$"])),
  case erlang_ls_tree:annotate_file(Include, Path) of
    {ok, FullName, _AnnotatedTree} ->
      {ok, FullName, #{ from => {1, 1}, to => {1, 1} }};
    {error, Error} ->
      {error, Error}
  end;
goto_definition(_Filename, #{ kind := include_lib, data := Include0 }, Path) ->
  Include = list_to_binary(lists:last(filename:split(string:trim(Include0, both, [$"])))),
  case erlang_ls_tree:annotate_file(Include, Path) of
    {ok, FullName, _AnnotatedTree} ->
      {ok, FullName, #{ from => {1, 1}, to => {1, 1} }};
    {error, Error} ->
      {error, Error}
  end;
goto_definition(Filename, #{ kind := type_application, data := Data }, Path) ->
  search(filename:basename(Filename), Path, definition(type_application, Data));
goto_definition(_Filename, _, _Path) ->
  {error, not_found}.

-spec goto_fun_definition(file:name_all(), [string()], erlang_ls_poi:kind(), any()) ->
  {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_fun_definition(Filename, Path, Kind, Data)  ->
  case erlang_ls_tree:annotate_file(Filename, Path) of
    {ok, FullName, AnnotatedTree} ->
      case erlang_ls_poi:match(AnnotatedTree, definition(Kind, Data)) of
        [] -> {error, not_found};
        Matches ->
          #{range := Range} = erlang_ls_poi:first(Matches),
          {ok, FullName, Range}
      end;
    {error, Error} ->
      {error, Error}
  end.

-spec definition(erlang_ls_poi:kind(), any()) -> {atom(), any()}.
definition(application, {_M, F, A}) ->
  {function, {F, A}};
definition(application, {F, A}) ->
  {function, {F, A}};
definition(implicit_fun, {_M, F, A}) ->
  {function, {F, A}};
definition(implicit_fun, {F, A}) ->
  {function, {F, A}};
definition(behaviour, Behaviour) ->
  {module, Behaviour};
definition(exports_entry, {F, A}) ->
  {function, {F, A}};
definition(import_entry, {_M, F, A}) ->
  {function, {F, A}};
definition(macro, Define) ->
  {define, Define};
definition(record_access, {Record, _Field}) ->
  {record, Record};
definition(record_expr, Record) ->
  {record, Record};
definition(type_application, {Type, _}) ->
  {type_definition, Type}.

-spec otp_path() -> [string()].
otp_path() ->
  {ok, Root} = erlang_ls_config:get(otp_path),
  resolve_paths( [ [Root, "lib", "*", "src"]
                 , [Root, "lib", "*", "include"]
                 ]).

-spec app_path() -> [string()].
app_path() ->
  {ok, RootUri} = erlang_ls_config:get(root_uri),
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  resolve_paths( [ [RootPath, "src"]
                 , [RootPath, "test"]
                 , [RootPath, "include"]
                 ]).

-spec deps_path() -> [string()].
deps_path() ->
  {ok, RootUri} = erlang_ls_config:get(root_uri),
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  {ok, Dirs} = erlang_ls_config:get(deps_dirs),
  Paths = [ resolve_paths( [ [RootPath, Dir, "src"]
                           , [RootPath, Dir, "test"]
                           , [RootPath, Dir, "include"]
                           ])
            || Dir <- Dirs
          ],
  lists:append(Paths).

-spec resolve_paths([[string()]]) -> [[string()]].
resolve_paths(PathSpecs) ->
  lists:append([resolve_path(PathSpec) || PathSpec <- PathSpecs]).

-spec resolve_path([string()]) -> [string()].
resolve_path(PathSpec) ->
  Path = filename:join(PathSpec),
  Paths = [[P | subdirs(P)] || P <- filelib:wildcard(Path)],
  lists:append(Paths).

%% Returns all subdirectories for the provided path
-spec subdirs(string()) -> [string()].
subdirs(Path) ->
  subdirs(Path, []).

-spec subdirs(string(), [string()]) -> [string()].
subdirs(Path, Subdirs) ->
  case file:list_dir(Path) of
    {ok, Files}     -> subdirs_(Path, Files, Subdirs);
    {error, enoent} -> Subdirs
  end.

-spec subdirs_(string(), [string()], [string()]) -> [string()].
subdirs_(Path, Files, Subdirs) ->
  Fold = fun(F, Acc) ->
             FullPath = filename:join([Path, F]),
             case filelib:is_dir(FullPath) of
               true  -> subdirs(FullPath, [FullPath | Acc]);
               false -> Acc
             end
         end,
  lists:foldl(Fold, Subdirs, Files).

-spec include_path() -> [string()].
include_path() ->
  lists:append( [ app_path(), otp_path(), deps_path() ]).

%% Look for a definition recursively in a file and its includes.
-spec search(binary(), [string()], any()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
search(Filename, Path, Thing) ->
  case erlang_ls_tree:annotate_file(Filename, Path) of
    {ok, FullName, AnnotatedTree} ->
      case find(AnnotatedTree, Thing) of
        {error, not_found} ->
          Includes = erlang_ls_poi:match_kind(AnnotatedTree, include),
          IncludeLibs = erlang_ls_poi:match_kind(AnnotatedTree, include_lib),
          search_in_includes(Includes ++ IncludeLibs, Path, Thing);
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

-spec search_in_includes([erlang_ls_poi:poi()], [string()], any()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
search_in_includes([], _Path, _Thing) ->
  {error, not_found};
search_in_includes([#{kind := Kind, data := Data}|T], Path, Thing) ->
  Include = normalize_include(Kind, Data),
  case search(list_to_binary(Include), Path, Thing) of
    {error, _Error} -> search_in_includes(T, Path, Thing);
    {ok, FullName, Range} -> {ok, FullName, Range}
  end.

-spec normalize_include(erlang_ls_poi:kind(), string()) -> string().
normalize_include(include, Include) ->
  string:trim(Include, both, [$"]);
normalize_include(include_lib, Include) ->
  lists:last(filename:split(string:trim(Include, both, [$"]))).

-spec filename(atom()) -> binary().
filename(Module) ->
  list_to_binary(atom_to_list(Module) ++ ".erl").
