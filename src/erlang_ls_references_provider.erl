-module(erlang_ls_references_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  ok.

-spec teardown() -> ok.
teardown() ->
  ok.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({references, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  case
    erlang_ls_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI | _] -> {find_references(Uri, POI), State};
    []        -> {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_references(binary(), erlang_ls_poi:poi()) -> any().
find_references(Uri, POI) ->
  case erlang_ls_references_index:get(Uri, POI) of
    []   -> null;
    Refs -> [location(U, R) || #{uri := U, range := R} <- Refs]
  end.

-spec location(uri(), erlang_ls_poi:range()) -> map().
location(Uri, Range) ->
  #{ uri   => Uri
   , range => erlang_ls_protocol:range(Range)
   }.
