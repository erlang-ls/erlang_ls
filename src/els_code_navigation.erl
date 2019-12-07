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
  FileName = filename:basename(Include),
  M = list_to_atom(FileName),
  case els_utils:find_module(M) of
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
  case els_dt_documents:lookup(Uri) of
    {ok, [#{document := Document}]} ->
      POIs = els_document:points_of_interest(Document, [Kind], Data),
      case POIs of
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

-spec include_uris(els_document:document()) -> [uri()].
include_uris(Document) ->
  POIs = els_document:points_of_interest( Document
                                              , [include, include_lib]),
  lists:foldl(fun add_include_uri/2, [], POIs).

-spec add_include_uri(poi(), [uri()]) -> [uri()].
add_include_uri(#{ id := String }, Acc) ->
  FileName = filename:basename(String),
  M = list_to_atom(FileName),
  case els_utils:find_module(M, hrl) of
    {ok, Uri}       -> [Uri | Acc];
    {error, _Error} -> Acc
  end.

-spec beginning() -> #{range => #{from => {1, 1}, to => {1, 1}}}.
beginning() ->
  #{range => #{from => {1, 1}, to => {1, 1}}}.
