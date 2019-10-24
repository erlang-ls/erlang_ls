%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(erlang_ls_code_navigation).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ goto_definition/2 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================

-spec goto_definition(uri(), erlang_ls_poi:poi()) ->
   {ok, uri(), erlang_ls_poi:poi()} | {error, any()}.
goto_definition( _Uri
               , #{ kind := Kind, data := {M, F, A} }
               ) when Kind =:= application;
                      Kind =:= implicit_fun ->
  {ok, Uri} = erlang_ls_db:find(completion_index, M),
  find(Uri, function, {F, A});
goto_definition( Uri
               , #{ kind := Kind, data := {F, A}}
               ) when Kind =:= application;
                      Kind =:= implicit_fun;
                      Kind =:= exports_entry ->
  find(Uri, function, {F, A});
goto_definition(_Uri, #{ kind := behaviour, data := Behaviour }) ->
  {ok, Uri} = erlang_ls_db:find(completion_index, Behaviour),
  find(Uri, module, Behaviour);
goto_definition(_Uri, #{ kind := import_entry, data := {M, F, A}}) ->
  {ok, Uri} = erlang_ls_db:find(completion_index, M),
  find(Uri, function, {F, A});
goto_definition(Uri, #{ kind := macro, data := Define }) ->
  find(Uri, define, Define);
goto_definition(Uri, #{ kind := record_access
                      , data := {Record, _}}) ->
  find(Uri, record, Record);
goto_definition(Uri, #{ kind := record_expr, data := Record }) ->
  find(Uri, record, Record);
goto_definition(_Uri, #{ kind := Kind, data := Include }
               ) when Kind =:= include;
                      Kind =:= include_lib ->
  %% TODO: Index header definitions as well
  FileName = include_filename(Kind, Include),
  M = list_to_atom(FileName),
  case erlang_ls_db:find(completion_index, M) of
    {ok, Uri} ->
      {ok, Uri, beginning()};
    not_found ->
      case erlang_ls_index:find_and_index_file(FileName) of
        {ok, Uri} ->
          {ok, Uri, beginning()};
        {error, Error} ->
          {error, Error}
      end
  end;
goto_definition(Uri, #{ kind := type_application, data := {Type, _} }) ->
  find(Uri, type_definition, Type);
goto_definition(_Filename, _) ->
  {error, not_found}.

%% TODO: Move poi/kind to hrl
-spec find([uri()], erlang_ls_poi:kind(), any()) ->
   {ok, uri(), erlang_ls_poi:poi()} | {error, not_found}.
find([], _Kind, _Data) ->
  {error, not_found};
find([Uri|Uris0], Kind, Data) ->
  case erlang_ls_db:find(documents, Uri) of
    {ok, Document} ->
      POIs = erlang_ls_document:points_of_interest(Document, [Kind], Data),
      case POIs of
        [] ->
          find(lists:usort(include_uris(Document) ++ Uris0), Kind, Data);
        Definitions ->
          {ok, Uri, lists:last(Definitions)}
      end;
    not_found ->
      find(Uris0, Kind, Data)
  end;
find(Uri, Kind, Data) ->
  find([Uri], Kind, Data).

-spec include_uris(erlang_ls_document:document()) -> [uri()].
include_uris(Document) ->
  POIs = erlang_ls_document:points_of_interest( Document
                                              , [include, include_lib]),
  lists:foldl(fun add_include_uri/2, [], POIs).

-spec add_include_uri(erlang_ls_poi:poi(), [uri()]) -> [uri()].
add_include_uri(#{ kind := Kind, data := String }, Acc) ->
  FileName = include_filename(Kind, String),
  M = list_to_atom(FileName),
  case erlang_ls_db:find(completion_index, M) of
    {ok, Uri} ->
      [Uri|Acc];
    not_found ->
      case erlang_ls_index:find_and_index_file(FileName) of
        {ok, Uri} ->
          [Uri|Acc];
        {error, _Error} ->
          Acc
      end
  end.

-spec include_filename('include' | 'include_lib', string()) -> string().
include_filename(include, String) ->
  string:trim(String, both, [$"]);
include_filename(include_lib, String) ->
  lists:last(filename:split(string:trim(String, both, [$"]))).

-spec beginning() -> #{range => #{from => {1, 1}, to => {1, 1}}}.
beginning() ->
  #{range => #{from => {1, 1}, to => {1, 1}}}.

%% TODO: Handle multiple header files with the same name?
