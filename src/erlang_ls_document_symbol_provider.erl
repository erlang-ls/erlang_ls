-module(erlang_ls_document_symbol_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
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
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  POIs = erlang_ls_document:points_of_interest(Document, [function]),
  lists:reverse([ #{ name => function_name(F, A)
                   , kind => ?SYMBOLKIND_FUNCTION
                   , location => #{ uri   => Uri
                                  , range => erlang_ls_protocol:range(Range)
                                  }
                   } || #{data := {F, A}, range := Range} <- POIs ]).

-spec function_name(atom(), non_neg_integer()) -> binary().
function_name(F, A) ->
  list_to_binary(io_lib:format("~p/~p", [F, A])).
