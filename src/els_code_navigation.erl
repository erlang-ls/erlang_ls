%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(els_code_navigation).

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

-spec goto_definition(uri(), poi()) ->
   {ok, uri(), poi()} | {error, any()}.
goto_definition( _Uri
               , #{ kind := Kind, id := {M, F, A} }
               ) when Kind =:= application;
                      Kind =:= implicit_fun;
                      Kind =:= import_entry ->
  case els_utils:find_module(M) of
    {ok, Uri}      -> find(Uri, function, {F, A});
    {error, Error} -> {error, Error}
  end;
goto_definition( Uri
               , #{ kind := Kind, id := {F, A}}
               ) when Kind =:= application;
                      Kind =:= implicit_fun;
                      Kind =:= export_entry ->
  find(Uri, function, {F, A});
goto_definition(_Uri, #{ kind := behaviour, id := Behaviour }) ->
  case els_utils:find_module(Behaviour) of
    {ok, Uri}      -> find(Uri, module, Behaviour);
    {error, Error} -> {error, Error}
  end;
goto_definition(Uri, #{ kind := macro, id := Define }) ->
  find(Uri, define, Define);
goto_definition(Uri, #{ kind := record_access
                      , id := {Record, _}}) ->
  find(Uri, record, Record);
goto_definition(Uri, #{ kind := record_expr, id := Record }) ->
  find(Uri, record, Record);
goto_definition(_Uri, #{ kind := Kind, id := Include }
               ) when Kind =:= include;
                      Kind =:= include_lib ->
  case els_utils:find_header(filename_to_atom(Include)) of
    {ok, Uri}      -> {ok, Uri, beginning()};
    {error, Error} -> {error, Error}
  end;
goto_definition(_Uri, #{ kind := type_application, id := {M, T, A} }) ->
  case els_utils:find_module(M) of
    {ok, Uri}      -> find(Uri, type_definition, {T, A});
    {error, Error} -> {error, Error}
  end;
goto_definition(Uri, #{ kind := type_application, id := {T, A} }) ->
  find(Uri, type_definition, {T, A});
goto_definition(_Filename, _) ->
  {error, not_found}.

-spec find(uri() | [uri()], poi_kind(), any()) ->
   {ok, uri(), poi()} | {error, not_found}.
find([], _Kind, _Data) ->
  {error, not_found};
find([Uri|Uris0], Kind, Data) ->
  case els_dt_document:lookup(Uri) of
    {ok, [Document]} ->
      POIs = els_dt_document:pois(Document, [Kind]),
      case [POI || #{id := Id} = POI <- POIs, Id =:= Data] of
        [] ->
          find(lists:usort(include_uris(Document) ++ Uris0), Kind, Data);
        Definitions ->
          {ok, Uri, hd(els_poi:sort(Definitions))}
      end;
    {ok, []} ->
      find(Uris0, Kind, Data)
  end;
find(Uri, Kind, Data) ->
  find([Uri], Kind, Data).

-spec include_uris(els_dt_document:item()) -> [uri()].
include_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  lists:foldl(fun add_include_uri/2, [], POIs).

-spec add_include_uri(els_dt_document:item(), [uri()]) -> [uri()].
add_include_uri(#{ id := String }, Acc) ->
  case els_utils:find_header(filename_to_atom(String)) of
    {ok, Uri}       -> [Uri | Acc];
    {error, _Error} -> Acc
  end.

-spec beginning() -> #{range => #{from => {1, 1}, to => {1, 1}}}.
beginning() ->
  #{range => #{from => {1, 1}, to => {1, 1}}}.

-spec filename_to_atom(string()) -> atom().
filename_to_atom(FileName) ->
  list_to_atom(filename:basename(FileName, filename:extension(FileName))).
