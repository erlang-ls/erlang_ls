-module(erlang_ls_doc).

-export([ get_doc/2 ]).

-spec get_doc(module(), binary()) -> binary().
get_doc(Module, Label) ->
  Which  = code:which(Module),
  [BF, BA] = re:split(Label, "/"),
  {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Which, [abstract_code]),
  case [erl_pp:form(Spec) ||
         {attribute, _, spec, {{F, A}, _}} = Spec <- AC
           , F =:= binary_to_atom(BF, utf8)
           , A =:= binary_to_integer(BA)] of
    [] ->
      <<"No specs available">>;
    [S] ->
      list_to_binary(S)
  end.
