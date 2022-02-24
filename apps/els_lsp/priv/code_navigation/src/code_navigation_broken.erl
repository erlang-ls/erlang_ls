-module(code_navigation_broken).

function_a() ->
  ok.

function_b() ->
  function_a() % missing comma, breaks parsing of this function!
  function_a(),
  case function_a() of
    ok ->
      function_a(),
      case function_a() of
        ok ->
          ok
      end
  end,
  function_a(
   ),
  function_a().
