-module(els_arg).
-export([new/2]).
-export([name/1]).
-export([name/2]).
-export([index/1]).
-export([merge_args/2]).

-export_type([arg/0]).
-export_type([args/0]).

-type args() :: [arg()].
-type arg() :: #{
    index := pos_integer(),
    name := string() | undefined | {type, string() | undefined},
    range => els_poi:poi_range()
}.

-spec new(pos_integer(), string()) -> arg().
new(Index, Name) ->
    #{index => Index, name => Name}.

-spec name(arg()) -> string().
name(Arg) ->
    name("Arg", Arg).

-spec name(string(), arg()) -> string().
name(Prefix, #{index := N, name := undefined}) ->
    Prefix ++ integer_to_list(N);
name(_Prefix, #{name := {type, Name}}) ->
    Name;
name(_Prefix, #{name := Name}) ->
    Name.

-spec index(arg()) -> string().
index(#{index := Index}) ->
    integer_to_list(Index).

-spec merge_args(args(), args()) -> args().
merge_args([], []) ->
    [];
merge_args([#{name := undefined} | T1], [Arg | T2]) ->
    [Arg | merge_args(T1, T2)];
merge_args(
    [#{name := {type, Name}} = Arg | T1],
    [#{name := undefined} | T2]
) ->
    [Arg#{name := Name} | merge_args(T1, T2)];
merge_args(
    [#{name := {type, _}} | T1],
    [Arg | T2]
) ->
    [Arg | merge_args(T1, T2)];
merge_args([Arg | T1], [_ | T2]) ->
    [Arg | merge_args(T1, T2)].
