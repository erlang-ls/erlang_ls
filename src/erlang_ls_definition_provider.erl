-module(erlang_ls_definition_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  #{}.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({definition, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  case
    erlang_ls_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI|_] ->
      Filename = erlang_ls_uri:path(Uri),
      case erlang_ls_code_navigation:goto_definition(Filename, POI) of
        {error, _Error} ->
          {null, State};
        {ok, FullName, Range} ->
          { #{ uri => erlang_ls_uri:uri(FullName)
             , range => erlang_ls_protocol:range(Range)
             }
          , State
          }
      end;
    [] ->
      {null, State}
  end.

-spec teardown() -> ok.
teardown() ->
  ok.
