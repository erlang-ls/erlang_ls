%%==============================================================================
%% EqWAlizer diagnostics
%%==============================================================================
-module(els_eqwalizer_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([
    is_default/0,
    run/1,
    source/0
]).

%% Exported to ease mocking during tests
-export([eqwalize/2]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> false.
is_default() ->
    false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    case filename:extension(Uri) of
        <<".erl">> ->
            Project = els_uri:path(els_config:get(root_uri)),
            Module = els_uri:module(Uri),
            %% Fully qualified call to ensure it's mockable
            lists:filtermap(fun make_diagnostic/1, ?MODULE:eqwalize(Project, Module));
        _ ->
            []
    end.

-spec source() -> binary().
source() ->
    <<"EqWAlizer">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec eqwalize(binary(), atom()) -> [string()].
eqwalize(Project, Module) ->
    Cmd = lists:flatten(
        io_lib:format("elp eqwalize ~p --format json-lsp --project ~s", [Module, Project])
    ),
    string:split(string:trim(os:cmd(Cmd)), "\n", all).

-spec make_diagnostic(binary()) -> {true, els_diagnostics:diagnostic()} | false.
make_diagnostic(Message) ->
    try json:decode(els_utils:to_binary(Message)) of
        #{
            <<"relative_path">> := _RelativePath,
            <<"diagnostic">> :=
                #{
                    <<"severity">> := Severity,
                    <<"message">> := Description,
                    <<"range">> :=
                        #{
                            <<"start">> :=
                                #{
                                    <<"line">> := FromLine,
                                    <<"character">> := FromChar
                                },
                            <<"end">> :=
                                #{
                                    <<"line">> := ToLine,
                                    <<"character">> := ToChar
                                }
                        }
                }
        } ->
            Range = els_protocol:range(#{
                from => {FromLine + 1, FromChar + 1},
                to => {ToLine + 1, ToChar + 1}
            }),
            Diagnostic = els_diagnostics:make_diagnostic(
                Range,
                Description,
                Severity,
                source()
            ),
            {true, Diagnostic};
        DecodedMessage ->
            ?LOG_WARNING("Unrecognized Eqwalizer diagnostic (~p)", [
                DecodedMessage
            ]),
            false
    catch
        C:E:St ->
            ?LOG_WARNING("Issue while running Eqwalizer (~p:~p:~p) for message ~p", [
                C, E, St, Message
            ]),
            false
    end.
