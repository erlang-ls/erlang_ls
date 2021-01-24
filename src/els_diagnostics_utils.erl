%%==============================================================================
%% Diagnostics utils
%%==============================================================================
-module(els_diagnostics_utils).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ dependencies/1
        , included_uris/1
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").
-include_lib("kernel/include/logger.hrl").

-spec dependencies(uri()) -> [atom()].
dependencies(Uri) ->
  dependencies([Uri], [], sets:new()).

-spec included_uris(els_dt_document:item()) -> [uri()].
included_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  included_uris([Id || #{id := Id} <- POIs], []).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec dependencies([uri()], [atom()], sets:set(binary())) -> [atom()].
dependencies([], Acc, _AlreadyProcessed) ->
  Acc;
dependencies([Uri|Uris], Acc, AlreadyProcessed) ->
  case els_dt_document:lookup(Uri) of
    {ok, [Document]} ->
      Behaviours = els_dt_document:pois(Document, [behaviour]),
      ParseTransforms = els_dt_document:pois(Document, [parse_transform]),
      IncludedUris = included_uris(Document),
      FilteredIncludedUris = exclude_already_processed( IncludedUris
                                                      , AlreadyProcessed
                                                      ),
      PTUris = lists:usort(
                 lists:flatten(
                   [pt_deps(Id) || #{id := Id} <- ParseTransforms])),
      FilteredPTUris = exclude_already_processed( PTUris
                                                , AlreadyProcessed
                                                ),
      dependencies( Uris ++ FilteredIncludedUris ++ FilteredPTUris
                  , Acc ++ [Id || #{id := Id} <- Behaviours ++ ParseTransforms]
                    ++ [els_uri:module(FPTUri) || FPTUri <- FilteredPTUris]
                  , sets:add_element(Uri, AlreadyProcessed));
    Error ->
      ?LOG_INFO("Lookup failed [Error=~p]", [Error]),
      []
  end.

-spec exclude_already_processed([uri()], sets:set()) -> [uri()].
exclude_already_processed(Uris, AlreadyProcessed) ->
  [Uri || Uri <- Uris, not sets:is_element(Uri, AlreadyProcessed)].

-spec pt_deps(atom()) -> [uri()].
pt_deps(Module) ->
  case els_utils:find_module(Module) of
    {ok, Uri} ->
      case els_dt_document:lookup(Uri) of
        {ok, [Document]} ->
          Applications = els_dt_document:pois(Document, [ application
                                                        , implicit_fun
                                                        ]),
          applications_to_uris(Applications);
        Error ->
          ?LOG_INFO("Lookup failed [Error=~p]", [Error]),
          []
      end;
    {error, Error} ->
      ?LOG_INFO("Find module failed [module=~p] [error=~p]", [Module, Error]),
      []
  end.

-spec applications_to_uris([poi()]) -> [uri()].
applications_to_uris(Applications) ->
  Modules = [M|| #{id := {M, _F, _A}} <- Applications],
  Fun = fun(M, Acc) ->
            case els_utils:find_module(M) of
              {ok, Uri} ->
                [Uri|Acc];
              {error, Error} ->
                ?LOG_INFO( "Could not find module [module=~p] [error=~p]"
                         , [M, Error]
                         ),
                Acc
            end
        end,
  lists:foldl(Fun, [], Modules).

-spec included_uris([atom()], [uri()]) -> [uri()].
included_uris([], Acc) ->
  lists:usort(Acc);
included_uris([Id|Ids], Acc) ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
    {ok, Uri}       -> included_uris(Ids, [Uri | Acc]);
    {error, _Error} -> included_uris(Ids, Acc)
  end.
