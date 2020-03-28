-module(els_code_lens_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        ]).

-include("erlang_ls.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{ resolveProvider => false }.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_codelens, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  Lenses = lenses(Uri),
  {Lenses, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec lenses(uri()) -> [els_code_lens:lens()].
lenses(Uri) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  lists:flatten(
    [els_code_lens:lenses(Id, Document) ||
      Id <- els_code_lens:enabled_lenses()]).
