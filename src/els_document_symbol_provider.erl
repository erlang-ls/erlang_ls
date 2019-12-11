-module(els_document_symbol_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({document_symbol, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  Functions = functions(Uri),
  case Functions of
    [] -> {null, State};
    _  -> {Functions, State}
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec functions(uri()) -> [map()].
functions(Uri) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_dt_document:pois(Document, [function]),
  lists:reverse([ #{ name => function_name(F, A)
                   , kind => ?SYMBOLKIND_FUNCTION
                   , location => #{ uri   => Uri
                                  , range => els_protocol:range(Range)
                                  }
                   } || #{id := {F, A}, range := Range} <- POIs ]).

-spec function_name(atom(), non_neg_integer()) -> binary().
function_name(F, A) ->
  list_to_binary(io_lib:format("~p/~p", [F, A])).
