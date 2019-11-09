-module(els_definition_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({definition, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:find_document(Uri),
  case
    els_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI|_] ->
      case els_code_navigation:goto_definition(Uri, POI) of
        {ok, DefUri, #{range := Range}} ->
          { #{ uri => DefUri, range => els_protocol:range(Range) }
          , State
          };
        _ ->
          {null, State}
      end;
    [] ->
      {null, State}
  end.
