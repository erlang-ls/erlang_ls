-module(erlang_ls_hover_provider).

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
handle_request({hover, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  case documentation(Uri, Line, Character) of
    <<>> ->
      {null, State};
    Doc ->
      {#{contents => Doc}, State}
  end.

-spec documentation(uri(), non_neg_integer(), non_neg_integer()) -> binary().
documentation(Uri, Line, Character) ->
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  case
    erlang_ls_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI|_] -> documentation(POI);
    []      -> <<>>
  end.

-spec documentation(poi()) -> binary().
documentation(#{kind := application, data := {M, F, A}}) ->
  case erlang_ls_db:find(specs_index, {M, F, A}) of
    {ok, Doc}          -> list_to_binary(erl_prettypr:format(Doc));
    {error, not_found} -> <<>>
  end;
documentation(_POI) ->
  <<>>.

%%==============================================================================
%% Internal functions
%%==============================================================================
