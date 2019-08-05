%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(erlang_ls_code_navigation).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ goto_definition/2 ]).

%% TODO: Refactor API
-export([ definition/1
        , search/3
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

%% TODO: Specify type instead of generic map
%% TODO: goto_definition/2 should probably not take the Uri, but a path.
-spec goto_definition(uri(), erlang_ls_poi:poi()) -> null | map().
goto_definition(_Uri, #{ info := {application, {M, _F, _A}} = Info }) ->
  case erlang_ls_tree:annotate_file(erlang_ls_uri:filename(M), full_path()) of
    {ok, Uri, AnnotatedTree} ->
      case erlang_ls_parser:find_poi_by_info(AnnotatedTree, definition(Info)) of
        [#{ range := Range }] ->
          %% TODO: Use API to create types
          #{ uri => Uri
           , range => erlang_ls_protocol:range(Range)
           };
        [] ->
          null
      end;
    {error, _Error} ->
      null
  end;
goto_definition(Uri, #{ info := {application, {_F, _A}} = Info }) ->
  case erlang_ls_tree:annotate_file(erlang_ls_uri:basename(Uri), full_path()) of
    {ok, Uri, AnnotatedTree} ->
      case erlang_ls_parser:find_poi_by_info(AnnotatedTree, definition(Info)) of
        [#{ range := Range }] ->
          %% TODO: Use API to create types
          #{ uri => Uri
           , range => erlang_ls_protocol:range(Range)
           };
        [] ->
          null
      end;
    {error, _Error} ->
      null
  end;
goto_definition(_Uri, #{ info := {behaviour, Behaviour} = Info }) ->
  Filename = erlang_ls_uri:filename(Behaviour),
  search(Filename, full_path(), definition(Info));
%% TODO: Eventually search everywhere and suggest a code lens to include a file
goto_definition(Uri, #{ info := {macro, _Define} = Info }) ->
  Filename = erlang_ls_uri:basename(Uri),
  search(Filename, app_path(), definition(Info));
goto_definition(Uri, #{ info := {record_expr, _Record} = Info }) ->
  Filename = erlang_ls_uri:basename(Uri),
  search(Filename, app_path(), Info);
goto_definition(_Uri, #{ info := {include, Include0} }) ->
  Include = list_to_binary(string:trim(Include0, both, [$"])),
  case erlang_ls_tree:annotate_file(Include, full_path()) of
    {ok, Uri, _AnnotatedTree} ->
      #{ uri => Uri
         %% TODO: We could point to the module attribute, instead
       , range => erlang_ls_protocol:range(#{ from => {0, 0}
                                            , to   => {0, 0}
                                            })
       };
    {error, _Error} ->
      null
  end;
goto_definition(_Uri, #{ info := {include_lib, Include0} }) ->
  Include = list_to_binary(lists:last(filename:split(string:trim(Include0, both, [$"])))),
  case erlang_ls_tree:annotate_file(Include, full_path()) of
    {ok, Uri, _AnnotatedTree} ->
      #{ uri => Uri
         %% TODO: We could point to the module attribute, instead
       , range => erlang_ls_protocol:range(#{ from => {0, 0}
                                            , to   => {0, 0}
                                            })
       };
    {error, _Error} ->
      null
  end;
goto_definition(_Uri, _) ->
  null.

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
-spec search(binary(), [string()], any()) -> null | map().
search(Filename, Path, Thing) ->
  case erlang_ls_tree:annotate_file(Filename, Path) of
    {ok, Uri, AnnotatedTree} ->
      case find(Uri, AnnotatedTree, Thing) of
        null ->
          Includes = erlang_ls_parser:find_poi_by_info_key(AnnotatedTree, include),
          IncludeLibs = erlang_ls_parser:find_poi_by_info_key(AnnotatedTree, include_lib),
          search_in_includes(Includes ++ IncludeLibs, Thing);
        Def ->
          Def
      end;
    {error, _Error} ->
      null
  end.

%% Look for a definition in a given tree
-spec find(uri(), erlang_ls_tree:tree(), any()) -> null | map().
find(Uri, AnnotatedTree, Thing) ->
  case erlang_ls_parser:find_poi_by_info(AnnotatedTree, Thing) of
    [#{ range := Range }|_] ->
      #{ uri => Uri, range => erlang_ls_protocol:range(Range) };
    [] ->
      null
  end.

-spec search_in_includes([erlang_ls_poi:poi()], string()) -> null | map().
search_in_includes([], _Thing) ->
  null;
search_in_includes([#{info := Info}|T], Thing) ->
  Include = normalize_include(Info),
  case search(list_to_binary(Include), app_path(), Thing) of
    null -> search_in_includes(T, Thing);
    Def  -> Def
  end.

-spec normalize_include({atom(), string()}) -> string().
normalize_include({include, Include}) ->
  string:trim(Include, both, [$"]);
normalize_include({include_lib, Include}) ->
  lists:last(filename:split(string:trim(Include, both, [$"]))).
