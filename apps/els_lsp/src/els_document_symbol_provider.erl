-module(els_document_symbol_provider).

-behaviour(els_provider).

-export([ is_enabled/0
        , handle_request/2
        ]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {response, any()}.
handle_request({document_symbol, Params}, _State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  Functions = functions(Uri),
  case Functions of
    [] -> {response, null};
    _  -> {response, Functions}
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
  els_utils:to_binary(io_lib:format("~p/~p", [F, A])).
