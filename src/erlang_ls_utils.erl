-module(erlang_ls_utils).

-export([halt/1]).

-spec halt(integer()) -> no_return().
halt(ExitCode) ->
  erlang:halt(ExitCode).
