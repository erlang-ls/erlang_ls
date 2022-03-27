%%==============================================================================
%% Edoc diagnostics
%%==============================================================================
-module(els_edoc_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  false.

%% The edoc application currently does not offer an API to
%% programmatically return a list of warnings and errors. Instead,
%% it simply outputs the warnings and errors to the standard
%% output.
%% We created an issue for the OTP team to address this:
%% https://github.com/erlang-ls/erlang_ls/issues/384
%% Meanwhile, hackity-hack!
%% Let's override the reporting module for edoc (edoc_report)
%% and (ab)use the process dictionary to collect the list of
%% warnings and errors.
-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      do_run(Uri);
    _ ->
      []
  end.

-spec do_run(uri()) -> [els_diagnostics:diagnostic()].
do_run(Uri) ->
  Paths = [els_utils:to_list(els_uri:path(Uri))],
  Fun = fun(Dir) ->
            Options = edoc_options(Dir),
            put(edoc_diagnostics, []),
            try
              edoc:run(Paths, Options)
            catch
              _:_:_ ->
                ok
            end,
            [make_diagnostic(L, Format, Args, Severity) ||
              {L, _Where, Format, Args, Severity}
                <- get(edoc_diagnostics), L =/= 0]
        end,
  tempdir:mktmp(Fun).

-spec source() -> binary().
source() ->
  <<"Edoc">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec edoc_options(string()) -> proplists:proplist().
edoc_options(Dir) ->
  Macros = [{N, V} || {'d', N, V} <- els_compiler_diagnostics:macro_options()],
  Includes = [I || {i, I} <- els_compiler_diagnostics:include_options()],
  [ {preprocess, true}
  , {macros, Macros}
  , {includes, Includes}
  , {dir, Dir}
  ].

-spec make_diagnostic(pos_integer(), string(), [any()], warning | error) ->
        els_diagnostics:diagnostic().
make_diagnostic(Line, Format, Args, Severity0) ->
  Severity = severity(Severity0),
  Message = els_utils:to_binary(io_lib:format(Format, Args)),
  els_diagnostics:make_diagnostic( els_protocol:range(#{ from => {Line, 1}
                                                       , to => {Line + 1, 1}
                                                       })
                                 , Message
                                 , Severity
                                 , source()).

-spec severity(warning | error) -> els_diagnostics:severity().
severity(warning) ->
  ?DIAGNOSTIC_WARNING;
severity(error) ->
  ?DIAGNOSTIC_ERROR.
