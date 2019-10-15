%%==============================================================================
%% Document gen_server
%%==============================================================================
-module(erlang_ls_document).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ create/2
        , set_text/2
        , get_completions/3
        , get_mfa/3
        , get_element_at_pos/3
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type document() :: #{ uri  := erlang_ls_uri:uri()
                     , text := binary()
                     , tree := any()
                     , pois := [erlang_ls_poi:poi()]
                     }.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec create(erlang_ls_uri:uri(), binary()) -> document().
create(Uri, Text) ->
  {ok, Tree, Extra} = erlang_ls_parser:parse(Text),
  AnnotatedTree = erlang_ls_tree:annotate(Tree, Extra),
  POIs = erlang_ls_tree:points_of_interest(AnnotatedTree),
  #{ uri  => Uri
   , text => Text
   , tree => AnnotatedTree
   , pois => POIs
   }.

-spec set_text(document(), binary()) -> document().
set_text(Document, Text) ->
  Document#{text := Text}.

-spec get_completions(document(), non_neg_integer(), non_neg_integer()) ->
  [map()].
get_completions(Document, Line, Column) ->
  Text            = maps:get(text, Document),
  LineText        = get_line_text(Text, Line),
  LineBeforeChar  = binary:part(LineText, {0, Column - 1}),
  Trigger         = binary:part(LineText, {Column - 1, 1}),
  %% TODO: Can't we get the context from the client?
  case Trigger of
    <<":">> ->
      {ok, Tokens, _} = erl_scan:string(binary_to_list(LineBeforeChar)),
      [H| _] = lists:reverse(Tokens),
      case H of
        {atom, _, Module} -> do_get_completions(Module)
      end;
    <<"#">> ->
      [#{label => list_to_binary(io_lib:format("~p", [RD]))} ||
        RD <- erlang_ls_completion:record_definitions()];
    _ ->
      []
  end.

-spec get_mfa(document(), non_neg_integer(), non_neg_integer()) ->
  {module(), atom(), non_neg_integer()}.
get_mfa(Document, Line, _Column) ->
  Text            = maps:get(text, Document),
  LineText        = get_line_text(Text, Line),
  {ok, Tokens, _} = erl_scan:string(binary_to_list(LineText)),
  [{atom, _, M}, {':', _}, {atom, _, F}, {'/', _}, {integer, _, A}] = Tokens,
  {M, F, A}.

-spec get_element_at_pos(document(), non_neg_integer(), non_neg_integer()) ->
  [any()].
get_element_at_pos(Document, Line, Column) ->
  AnnotatedTree = maps:get(tree, Document),
  erlang_ls_poi:match_pos(AnnotatedTree, {Line, Column}).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec get_line_text(binary(), integer()) -> binary().
get_line_text(Text, Line) ->
  Lines = binary:split(Text, <<"\n">>, [global]),
  lists:nth(Line + 1, Lines).

-spec function_name_to_binary(atom(), non_neg_integer()) -> binary().
function_name_to_binary(Function, Arity) ->
  list_to_binary(io_lib:format("~p/~p", [Function, Arity])).

-spec do_get_completions(module()) -> [map()].
do_get_completions(Module) ->
  try Module:module_info(exports) of
      Info ->
      CS = [{Module, function_name_to_binary(F, A)} || {F, A} <- Info],
      [#{ label => C
        , data => M
        , documentation => erlang_ls_doc:get_doc(M, C)
        } || {M, C} <- CS]
  catch _:_ ->
      []
  end.
