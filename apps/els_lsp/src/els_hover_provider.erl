%%==============================================================================
%% Hover Provider
%%==============================================================================
-module(els_hover_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({hover, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Doc} = els_utils:lookup_document(Uri),
  case els_dt_document:get_element_at_pos(Doc, Line + 1, Character + 1) of
    [] ->
      {null, State};
    [POI|_] ->
      Entries = els_docs:docs(els_uri:module(Uri), POI),
      {#{contents => els_markup_content:new(Entries)}, State}
  end.
