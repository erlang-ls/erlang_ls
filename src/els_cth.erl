-module(els_cth).

-export([init/2]).

-export([ on_tc_fail/4
        , on_tc_skip/4
        ]).

-include("erlang_ls.hrl").

-type id() :: any().
-type options() :: #{uri := uri(), line := pos_integer()}.
-type suite() :: atom().
-type testcase() :: atom() | tuple().
-type state() :: #{options := options()}.
-type skip_reason() :: {tc_auto_skip | tc_user_skip, any()}.
-type fail_reason() :: any().
-type reason() :: skip_reason() | fail_reason().

-spec init(id(), options()) -> {ok, state()}.
init(_Id, Opts) ->
  {ok, #{options => Opts}}.

-spec on_tc_fail(suite(), testcase(), fail_reason(), state()) -> state().
on_tc_fail(_Suite, _TestCase, Reason, State) ->
  publish_result(Reason, State),
  State.

-spec on_tc_skip(suite(), testcase(), skip_reason(), state()) -> state().
on_tc_skip(_Suite, _TestCase, {_ReasonType, Reason}, State) ->
  publish_result(Reason, State),
  State.

-spec publish_result(reason(), state()) -> ok.
publish_result(Reason, #{options := #{uri := Uri, line := Line}}) ->
  Message = els_utils:to_binary(io_lib:format("~p", [Reason])),
  els_command_ct_run_test:publish_result(Uri, Line, ?DIAGNOSTIC_ERROR, Message).
