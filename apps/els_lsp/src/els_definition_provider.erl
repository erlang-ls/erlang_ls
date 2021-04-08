-module(els_definition_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({definition, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1),
  {goto_definition(Uri, POIs), State}.

-spec goto_definition(uri(), [poi()]) -> map() | null.
goto_definition(_Uri, []) ->
  null;
goto_definition(Uri, [POI|Rest]) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, DefUri, #{range := Range}} ->
      #{uri => DefUri, range => els_protocol:range(Range)};
    _ ->
      goto_definition(Uri, Rest)
  end.
