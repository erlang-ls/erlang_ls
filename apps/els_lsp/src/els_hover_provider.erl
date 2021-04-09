%%==============================================================================
%% Hover Provider
%%==============================================================================
-module(els_hover_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("els_lsp.hrl").

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
  POIs = els_dt_document:get_element_at_pos(Doc, Line + 1, Character + 1),
  {get_docs(Uri, POIs), State}.

-spec get_docs(uri(), [poi()]) -> map() | null.
get_docs(_Uri, []) ->
  null;
get_docs(Uri, [POI|Rest]) ->
  case els_docs:docs(Uri, POI) of
    [] ->
      get_docs(Uri, Rest);
    Entries ->
      #{contents => els_markup_content:new(Entries)}
  end.
