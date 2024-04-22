-module(els_arg).
-export([name/1]).
-export([name/2]).
-export([index/1]).
-export_type([arg/0]).

-type arg() :: #{
    index := pos_integer(),
    name := string() | undefined,
    range => els_poi:poi_range()
}.

-spec name(arg()) -> string().
name(Arg) ->
    name("Arg", Arg).

-spec name(string(), arg()) -> string().
name(Prefix, #{index := N, name := undefined}) ->
    Prefix ++ integer_to_list(N);
name(_Prefix, #{name := Name}) ->
    Name.

-spec index(arg()) -> string().
index(#{index := Index}) ->
    integer_to_list(Index).
