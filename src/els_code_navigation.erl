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
goto_definition( _Uri
               , #{ kind := Kind, id := Module }
               ) when Kind =:= atom;
                      Kind =:= behaviour;
                      Kind =:= module ->
  case els_utils:find_module(Module) of
    {ok, Uri}      -> find(Uri, module, Module);
    {error, Error} -> {error, Error}
  end;
goto_definition(Uri, #{ kind := macro, id := Define }) ->
  find(Uri, define, Define);
goto_definition(Uri, #{ kind := record_access
                      , id := Record}) ->
  find(Uri, record, Record);
goto_definition(Uri, #{ kind := record_expr, id := Record }) ->
  find(Uri, record, Record);
goto_definition(_Uri, #{ kind := Kind, id := Id }
               ) when Kind =:= include;
                      Kind =:= include_lib ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
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
find(UriOrUris, Kind, Data) ->
  find(UriOrUris, Kind, Data, sets:new()).

-spec find(uri() | [uri()], poi_kind(), any(), sets:set(binary())) ->
   {ok, uri(), poi()} | {error, not_found}.
find([], _Kind, _Data, _AlreadyVisited) ->
  {error, not_found};
find([Uri|Uris0], Kind, Data, AlreadyVisited) ->
  case sets:is_element(Uri, AlreadyVisited) of
    true ->
      find(Uris0, Kind, Data, AlreadyVisited);
    false ->
      AlreadyVisited2 = sets:add_element(Uri, AlreadyVisited),
      case els_dt_document:lookup(Uri) of
        {ok, [Document]} ->
          find_in_document([Uri|Uris0], Document, Kind, Data, AlreadyVisited2);
        {ok, []} ->
          find(Uris0, Kind, Data, AlreadyVisited2)
      end
  end;
find(Uri, Kind, Data, AlreadyVisited) ->
  find([Uri], Kind, Data, AlreadyVisited).

-spec find_in_document(uri() | [uri()], els_dt_document:item(), poi_kind()
                      , any(), sets:set(binary())) ->
        {ok, uri(), poi()} | {error, any()}.
find_in_document([Uri|Uris0], Document, Kind, Data, AlreadyVisited) ->
  POIs = els_dt_document:pois(Document, [Kind]),
  case [POI || #{id := Id} = POI <- POIs, Id =:= Data] of
    [] ->
      case maybe_imported(Document, Kind, Data) of
        {ok, U, P} -> {ok, U, P};
        {error, not_found} ->
          find(lists:usort(include_uris(Document) ++ Uris0), Kind, Data,
               AlreadyVisited)
      end;
    Definitions ->
      {ok, Uri, hd(els_poi:sort(Definitions))}
  end.

-spec include_uris(els_dt_document:item()) -> [uri()].
include_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  lists:foldl(fun add_include_uri/2, [], POIs).

-spec add_include_uri(els_dt_document:item(), [uri()]) -> [uri()].
add_include_uri(#{ id := Id }, Acc) ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
    {ok, Uri}       -> [Uri | Acc];
    {error, _Error} -> Acc
  end.

-spec beginning() -> #{range => #{from => {1, 1}, to => {1, 1}}}.
beginning() ->
  #{range => #{from => {1, 1}, to => {1, 1}}}.

%% @doc check for a match in any of the module imported functions.
-spec maybe_imported(els_dt_document:item(), poi_kind(), any()) ->
        {ok, uri(), poi()} | {error, not_found}.
maybe_imported(Document, function, {F, A}) ->
  POIs = els_dt_document:pois(Document, [import_entry]),
  case [{M, F, A} || #{id := {M, FP, AP}} <- POIs, FP =:= F, AP =:= A] of
    [] -> {error, not_found};
    [{M, F, A}|_] ->
      case els_utils:find_module(M) of
        {ok, Uri0}      -> find(Uri0, function, {F, A});
        {error, Error} -> {error, Error}
      end
  end;
maybe_imported(_Document, _Kind, _Data) ->
  {error, not_found}.
