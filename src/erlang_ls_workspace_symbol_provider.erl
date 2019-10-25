-module(erlang_ls_workspace_symbol_provider).

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
handle_request({symbol, Params}, State) ->
  %% TODO: Version 3.15 of the protocol introduces a much nicer way of
  %%       specifying queries, allowing clients to send the symbol kind.
  %%       For now, let's use the following convention for queries:
  %%
  %%       m     -> Request the list of all modules
  %%       m-foo -> Request the module 'foo'
  #{ <<"query">> := Query} = Params,
  Symbols =
    case Query of
      <<"m">> ->
        case modules() of
          [] ->
            null;
          Modules -> Modules
        end;
      <<"m-", Binary/binary>> ->
        Module = binary_to_atom(Binary, utf8),
        case erlang_ls_db:find(completion_index, Module) of
          {ok, Uri} ->
            [symbol_information({Module, Uri})];
          not_found ->
            null
        end;
      _ ->
        null
    end,
  {Symbols, State}.

-spec teardown() -> ok.
teardown() ->
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec modules() -> [symbol_information()].
modules() ->
  %% TODO: Stop indexing header files together with modules
  All = erlang_ls_db:list(completion_index),
  F = fun({_Module, Uri}) ->
          filename:extension(Uri) =:= <<".erl">>
      end,
  lists:map(fun symbol_information/1, lists:filter(F, All)).

-spec symbol_information({atom(), uri()}) -> symbol_information().
symbol_information({Module, Uri}) ->
  Range = #{from => {1, 1}, to => {1, 1}},
  #{ name => atom_to_list(Module)
   , kind => ?SYMBOLKIND_MODULE
   , location => #{ uri => Uri
                  , range => erlang_ls_protocol:range(Range)
                  }
   }.
