-module(els_io_string).

-export([new/1]).

-export([ start_link/1
        , init/1
        , loop/1
        , skip/3
        ]).

-type state() :: #{ buffer   := string()
                  , original := string()
                  }.

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec new(string() | binary()) -> pid().
new(Str) when is_binary(Str) ->
  new(els_utils:to_list(Str));
new(Str) ->
  start_link(Str).

%%------------------------------------------------------------------------------
%% IO server
%%
%% Implementation of a subset of the io protocol in order to only support
%% reading operations.
%%------------------------------------------------------------------------------

-spec start_link(string()) -> pid().
start_link(Str) ->
  spawn_link(?MODULE, init, [Str]).

-spec init(string()) -> ok.
init(Str) ->
  State = #{buffer => Str, original => Str},
  ?MODULE:loop(State).

-spec loop(state()) -> ok.
loop(#{buffer := Str} = State) ->
  receive
    {io_request, From, ReplyAs, Request} ->
      {Reply, NewStr} = request(Request, Str),
      reply(From, ReplyAs, Reply),
      ?MODULE:loop(State#{buffer := NewStr});
    {file_request, From, Ref, close} ->
      file_reply(From, Ref, ok);
    {file_request, From, Ref, {position, Pos}} ->
      {Reply, NewState} = file_position(Pos, State),
      file_reply(From, Ref, Reply),
      ?MODULE:loop(NewState);
    _Unknown ->
      ?MODULE:loop(State)
  end.

-spec reply(pid(), pid(), any()) -> any().
reply(From, ReplyAs, Reply) ->
  From ! {io_reply, ReplyAs, Reply}.

-spec file_reply(pid(), pid(), any()) -> any().
file_reply(From, ReplyAs, Reply) ->
  From ! {file_reply, ReplyAs, Reply}.

-spec file_position(integer(), state()) -> {any(), state()}.
file_position(Pos, #{original := Original} = State) ->
  Buffer = lists:nthtail(Pos, Original),
  {{ok, Pos}, State#{buffer => Buffer}}.

-spec request(any(), string()) -> {string() | {error, request}, string()}.
request({get_chars, _Encoding, _Prompt, N}, Str) ->
  get_chars(N, Str);
request({get_line, _Encoding, _Prompt}, Str) ->
  get_line(Str);
request({get_until, _Encoding, _Prompt, Module, Function, Xargs}, Str) ->
  get_until(Module, Function, Xargs, Str);
request(_Other, State) ->
  {{error, request}, State}.

-spec get_chars(integer(), string()) -> {string() | eof, string()}.
get_chars(_N, []) ->
  {eof, []};
get_chars(1, [Ch | Str]) ->
  {[Ch], Str};
get_chars(N, Str) ->
  do_get_chars(N, Str, []).

-spec do_get_chars(integer(), string(), string()) -> {string(), string()}.
do_get_chars(0, Str, Result) ->
  {lists:flatten(Result), Str};
do_get_chars(_N, [], Result) ->
  {Result, []};
do_get_chars(N, [Ch | NewStr], Result) ->
  do_get_chars(N - 1, NewStr, [Result, Ch]).

-spec get_line(string()) -> {string() | eof, string()}.
get_line([]) ->
  {eof, []};
get_line(Str) ->
  do_get_line(Str, []).

-spec do_get_line(string(), string()) -> {string() | eof, string()}.
do_get_line([], Result) ->
  {lists:flatten(Result), []};
do_get_line("\r\n" ++ RestStr, Result) ->
  {lists:flatten(Result), RestStr};
do_get_line("\n" ++ RestStr, Result) ->
  {lists:flatten(Result), RestStr};
do_get_line("\r" ++ RestStr, Result) ->
  {lists:flatten(Result), RestStr};
do_get_line([Ch | RestStr], Result) ->
  do_get_line(RestStr, [Result, Ch]).

-spec get_until(module(), atom(), list(), term()) ->
  {term(), string()}.
get_until(Module, Function, XArgs, Str) ->
  apply_get_until(Module, Function, [], Str, XArgs).

-spec apply_get_until(module(), atom(), any(), string() | eof, list()) ->
  {term(), string()}.
apply_get_until(Module, Function, State, String, XArgs) ->
  case apply(Module, Function, [State, String | XArgs]) of
    {done, Result, NewStr} ->
      {Result, NewStr};
    {more, NewState} ->
      apply_get_until(Module, Function, NewState, eof, XArgs)
  end.

-spec skip(string() | {cont, integer(), string()}, term(), integer()) ->
  {more, {cont, integer(), string()}} | {done, integer(), string()}.
skip(Str, _Data, Length) when is_list(Str) ->
  {more, {cont, Length, Str}};
skip({cont, 0, Str}, _Data, Length) ->
  {done, Length, Str};
skip({cont, Length, []}, _Data, Length) ->
  {done, eof, []};
skip({cont, Length, [_ | RestStr]}, _Data, _Length) ->
  {more, {cont, Length - 1, RestStr}}.
