-module(erlang_ls_document_symbol_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

-include("erlang_ls.hrl").

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
handle_request({document_symbol, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  POIs = erlang_ls_document:points_of_interest(Document),
  case POIs of
    [] -> {null, State};
    _ ->
      {[ #{ name => list_to_binary(io_lib:format("~p/~p", [F, A]))
          , kind => ?SYMBOLKIND_FUNCTION
          , location => #{ uri   => Uri
                         , range => erlang_ls_protocol:range(Range)
                         }
          }
         || #{ info := {function, {F, A}}
             , range := Range
             } <- POIs ], State}
  end.

-spec teardown() -> ok.
teardown() ->
  ok.
