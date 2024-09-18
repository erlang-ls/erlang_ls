-module(undefined_record_suggest).

-record(foobar, {foobar}).

function_a(R) ->
    #foo_bar{} = R,
    R#foobar.foobaz.
