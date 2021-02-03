-module(diagnostics_macros).

-spec main() -> ok
main() ->
  case ?DEFINED_WITHOUT_VALUE of
    true ->
      case ?DEFINED_WITH_VALUE of
        1 ->
          ?UNDEFINED
      end
  end.
