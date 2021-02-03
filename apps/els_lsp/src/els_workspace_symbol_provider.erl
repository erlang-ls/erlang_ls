-module(els_workspace_symbol_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("els_lsp.hrl").

-define(LIMIT, 100).

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({symbol, Params}, State) ->
  %% TODO: Version 3.15 of the protocol introduces a much nicer way of
  %%       specifying queries, allowing clients to send the symbol kind.
  #{<<"query">> := Query} = Params,
  TrimmedQuery = string:trim(Query),
  {modules(TrimmedQuery), State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec modules(binary()) -> [symbol_information()].
modules(<<>>) ->
  [];
modules(Query) ->
  {ok, All} = els_dt_document_index:find_by_kind(module),
  Compare   = fun(#{id := X}, #{id := Y}) -> X < Y end,
  AllSorted = lists:sort(Compare, All),
  {ok, RePattern} = re:compile(Query),
  ReOpts = [{capture, none}],
  F = fun(Name) ->
          re:run(Name, RePattern, ReOpts) =:= match
      end,
  Filtered = filter_with_limit(AllSorted, F, ?LIMIT, []),
  [symbol_information(X) || X <- Filtered].

-spec symbol_information({binary(), uri()}) -> symbol_information().
symbol_information({Name, Uri}) ->
  Range = #{from => {1, 1}, to => {1, 1}},
  #{ name => Name
   , kind => ?SYMBOLKIND_MODULE
   , location => #{ uri => Uri
                  , range => els_protocol:range(Range)
                  }
   }.

-spec filter_with_limit( [els_dt_document_index:item()]
                       , function()
                       , integer()
                       , [{binary(), uri()}]) ->
  [{binary(), uri()}].
filter_with_limit(_Modules, _Filter, 0, Result) ->
  lists:reverse(Result);
filter_with_limit([], _Filter, _Limit, Result) ->
  lists:reverse(Result);
filter_with_limit([Item | Items], Filter, Limit, Result) ->
  #{kind := module, id := Module, uri := Uri} = Item,
  Name = atom_to_binary(Module, utf8),
  case Filter(Name) of
    true  ->
      filter_with_limit(Items, Filter, Limit - 1, [{Name, Uri} | Result]);
    false ->
      filter_with_limit(Items, Filter, Limit, Result)
  end.
