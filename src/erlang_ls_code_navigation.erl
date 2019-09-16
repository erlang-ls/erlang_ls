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

-export([ otp_path/0
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
  goto_definition(Filename, POI, full_path()).

%% TODO: Abstract pattern
-spec goto_definition(binary(), erlang_ls_poi:poi(), [string()]) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_definition( _Filename
               , #{ info := {Type, {M, _F, _A}} = Info }
               , Path
               ) when Type =:= application;
                      Type =:= implicit_fun ->
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
               , #{ info := {Type, {_F, _A}} = Info }
               , Path
               ) when Type =:= application;
                      Type =:= implicit_fun;
                      Type =:= exports_entry ->
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
goto_definition( _Filename
               , #{ info := {import_entry, {M, _F, _A}} = Info }
               , Path) ->
  search(filename(M), Path, definition(Info));
%% TODO: Eventually search everywhere and suggest a code lens to include a file
goto_definition(Filename, #{ info := {macro, _Define} = Info }, Path) ->
  search(filename:basename(Filename), Path, definition(Info));
goto_definition(Filename, #{ info := {record_access, {_Record, _Var}} = Info }, Path) ->
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
goto_definition(Filename, #{ info := {type_application, _Type} = Info }, Path) ->
  search(filename:basename(Filename), Path, definition(Info));
goto_definition(_Filename, _, _Path) ->
  {error, not_found}.

-spec definition({atom(), any()}) -> {atom(), any()}.
definition({application, {_M, F, A}}) ->
  {function, {F, A}};
definition({application, {F, A}}) ->
  {function, {F, A}};
definition({implicit_fun, {_M, F, A}}) ->
  {function, {F, A}};
definition({implicit_fun, {F, A}}) ->
  {function, {F, A}};
definition({behaviour, Behaviour}) ->
  {module, Behaviour};
definition({exports_entry, {F, A}}) ->
  {function, {F, A}};
definition({import_entry, {_M, F, A}}) ->
  {function, {F, A}};
definition({macro, Define}) ->
  {define, Define};
definition({record_access, {Record, _Variable}}) ->
  {record, Record};
definition({record_expr, Record}) ->
  {record, Record};
definition({type_application, {Type, _}}) ->
  {type_definition, Type}.

-spec otp_path() -> [string()].
otp_path() ->
  {ok, Root} = erlang_ls_buffer_server:get_otp_path(),
  Sources = filename:join([Root, "lib", "*", "src"]),
  Includes = filename:join([Root, "lib", "*", "include"]),
  lists:append([ filelib:wildcard(Sources)
               , filelib:wildcard(Includes)
               ]).

-spec app_path() -> [string()].
app_path() ->
  {ok, RootUri} = erlang_ls_buffer_server:get_root_uri(),
  RootPath = erlang_ls_uri:path(RootUri),
  [ filename:join([RootPath, "src"])
  , filename:join([RootPath, "include"])
  ].

-spec full_path() -> [string()].
full_path() ->
  lists:append( [ app_path(), otp_path() ]).

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
search_in_includes([#{info := Info}|T], Path, Thing) ->
  Include = normalize_include(Info),
  case search(list_to_binary(Include), Path, Thing) of
    {error, _Error} -> search_in_includes(T, Path, Thing);
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
