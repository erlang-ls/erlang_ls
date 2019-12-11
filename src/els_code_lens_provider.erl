-module(els_code_lens_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{ resolveProvider => false }.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({document_codelens, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  Lenses = lenses(Uri),
  {Lenses, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec lenses(uri()) -> [map()].
lenses(Uri) ->
  {ok, _Document} = els_utils:lookup_document(Uri),
  Lenses = [#{ range => one_line_range(3)
             , command =>
                   make_command(<<"Foo Title">>, <<"foo command">>, [])}],
  lager:info("lenses: [Lenses=~p]", [Lenses]),
  Lenses.

-spec one_line_range(non_neg_integer()) -> range().
one_line_range(Line) ->
  Range   = #{from => {Line, 1}, to => {Line + 1, 1}},
  els_protocol:range(Range).

-spec make_command(binary(), binary(), [any()]) -> command().
make_command(Title, Command, Args) ->
    #{ title => Title
     , command => Command
     , arguments => Args
     }.
