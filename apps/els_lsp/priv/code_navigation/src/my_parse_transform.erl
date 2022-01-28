-module(my_parse_transform).

-export([ parse_transform/2 ]).

-export([ format_error/1 ]).

parse_transform(_Forms, _Options) ->
  [{error, {42, ?MODULE, custom_description}}].

format_error(Desc) ->
    lists:flatten(io_lib:format("~p", [Desc])).
