-module(els_references_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({references, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:find_document(Uri),
  case
    els_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI | _] -> {find_references(Uri, POI), State};
    []        -> {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_references(binary(), poi()) -> any().
find_references(Uri, #{ kind := Kind
                      , id   := Id
                      }) when Kind =:= application;
                              Kind =:= implicit_fun;
                              Kind =:= function;
                              Kind =:= exports_entry ->
  Key = case Id of
          {F, A}    -> {els_uri:module(Uri), F, A};
          {M, F, A} -> {M, F, A}
        end,
  case els_db:find(references, Key) of
    {error, not_found} ->
      null;
    {ok, []} ->
      null;
    {ok, Refs} ->
      [location(U, R) || #{uri := U, range := R} <- ordsets:to_list(Refs)]
  end;
find_references(_Uri, _POI) ->
  null.

-spec location(uri(), poi_range()) -> map().
location(Uri, Range) ->
  #{ uri   => Uri
   , range => els_protocol:range(Range)
   }.
