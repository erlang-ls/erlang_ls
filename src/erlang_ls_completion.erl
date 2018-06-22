-module(erlang_ls_completion).

-export([ record_definitions/0
        ]).

%% TODO: Store these in an ETS table
-spec record_definitions() -> [atom()].
record_definitions() ->
  lists:usort(lists:flatten([record_definitions(B) || B <- all_beams()])).

-spec record_definitions(file:filename() | atom()) -> [atom()].
record_definitions(File) ->
  [D || {attribute, _, record, {D, _}} <- record_attributes(forms(File))].

-spec record_attributes([tuple()]) -> [tuple()].
record_attributes(Forms) ->
  [A || {attribute, _, record, _} = A <- Forms].

-spec forms(file:filename()) -> [tuple()].
forms(File) ->
  case beam_lib:chunks(File, [abstract_code]) of
    {ok, {_, [{abstract_code, {_, Forms}}]}} ->
      Forms;
    _ ->
      []
  end.

-spec all_beams() -> [file:filename()].
all_beams() ->
  [beam(L) || L <- code:all_loaded()].

-spec beam(tuple()) -> file:filename().
beam({M, preloaded}) ->
  {_, _, F} = code:get_object_code(M),
  F;
beam({_M, F}) when is_list(F) ->
  F.
