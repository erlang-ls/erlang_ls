%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_elvis_diagnostics).

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

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
    true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    case els_utils:project_relative(Uri) of
        {error, not_relative} ->
            [];
        RelFile ->
            %% Note: elvis_core:rock_this requires a file path relative to the
            %%       project root, formatted as a string.
            try
                Filename = get_elvis_config_path(),
                Config = elvis_config:from_file(Filename),
                elvis_core:rock_this(RelFile, Config)
            of
                ok -> [];
                {fail, Problems} -> lists:flatmap(fun format_diagnostics/1, Problems)
            catch
                Err ->
                    ?LOG_WARNING("Elvis error.[Err=~p] ", [Err]),
                    []
            end
    end.

-spec source() -> binary().
source() ->
    <<"Elvis">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec format_diagnostics(any()) -> [map()].
format_diagnostics(#{file := _File, rules := Rules}) ->
    R = format_rules(Rules),
    lists:flatten(R).

%%% This section is based directly on elvis_result:print_rules
-spec format_rules([any()]) -> [[map()]].
format_rules([]) ->
    [];
format_rules([#{error_msg := Msg, info := Info} | Items]) ->
    [
        diagnostic(<<"Config Error">>, Msg, 1, Info, ?DIAGNOSTIC_ERROR)
        | format_rules(Items)
    ];
format_rules([#{warn_msg := Msg, info := Info} | Items]) ->
    [
        diagnostic(<<"Config Warning">>, Msg, 1, Info, ?DIAGNOSTIC_WARNING)
        | format_rules(Items)
    ];
format_rules([#{items := []} | Items]) ->
    format_rules(Items);
format_rules([#{items := Items, name := Name} | EItems]) ->
    ItemDiags = format_item(Name, Items),
    [lists:flatten(ItemDiags) | format_rules(EItems)].

%% Item
-spec format_item(any(), [any()]) -> [[map()]].
format_item(Name, [#{message := Msg, line_num := Ln, info := Info} | Items]) ->
    Diagnostic = diagnostic(Name, Msg, Ln, Info, ?DIAGNOSTIC_WARNING),
    [Diagnostic | format_item(Name, Items)];
format_item(_Name, []) ->
    [].

%%% End of section based directly on elvis_result:print_rules

-spec diagnostic(
    any(),
    any(),
    integer(),
    [any()],
    els_diagnostics:severity()
) -> [map()].
diagnostic(Name, Msg, Ln, Info, Severity) ->
    %% Avoid negative line numbers
    DiagLine = make_protocol_line(Ln),
    FMsg = io_lib:format(Msg, Info),
    Range = els_protocol:range(#{
        from => {DiagLine, 1},
        to => {DiagLine + 1, 1}
    }),
    Message = els_utils:to_binary(FMsg),
    [
        #{
            range => Range,
            severity => Severity,
            code => Name,
            source => source(),
            message => Message,
            relatedInformation => []
        }
    ].

-spec make_protocol_line(Line :: number()) -> number().
make_protocol_line(Line) when Line =< 0 ->
    1;
make_protocol_line(Line) ->
    Line.

-spec get_elvis_config_path() -> file:filename_all().
get_elvis_config_path() ->
    case els_config:get(elvis_config_path) of
        undefined ->
            RootPath = els_uri:path(els_config:get(root_uri)),
            filename:join([RootPath, "elvis.config"]);
        FilePath ->
            FilePath
    end.
