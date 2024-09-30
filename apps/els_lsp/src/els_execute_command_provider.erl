-module(els_execute_command_provider).

-behaviour(els_provider).

-export([
    options/0,
    handle_request/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec options() -> map().
options() ->
    Commands = [
        <<"server-info">>,
        <<"ct-run-test">>,
        <<"show-behaviour-usages">>,
        <<"suggest-spec">>,
        <<"function-references">>,
        <<"refactor.extract">>,
        <<"add-behaviour-callbacks">>,
        <<"bump-variables">>
    ],
    #{
        commands => [
            els_command:with_prefix(Cmd)
         || Cmd <- Commands ++ wrangler_handler:enabled_commands()
        ]
    }.

-spec handle_request(any()) -> {response, any()}.
handle_request({workspace_executecommand, Params}) ->
    #{<<"command">> := PrefixedCommand} = Params,
    Arguments = maps:get(<<"arguments">>, Params, []),
    Result = execute_command(
        els_command:without_prefix(PrefixedCommand),
        Arguments
    ),
    {response, Result}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(<<"server-info">>, _Arguments) ->
    {ok, Version} = application:get_key(?APP, vsn),
    BinVersion = list_to_binary(Version),
    Root = filename:basename(els_uri:path(els_config:get(root_uri))),
    ConfigPath =
        case els_config:get(config_path) of
            undefined -> <<"undefined">>;
            Path -> list_to_binary(Path)
        end,

    OtpPathConfig = list_to_binary(els_config:get(otp_path)),
    OtpRootDir = list_to_binary(code:root_dir()),
    OtpMessage =
        case OtpRootDir == OtpPathConfig of
            true ->
                <<", OTP root ", OtpRootDir/binary>>;
            false ->
                <<", OTP root(code):", OtpRootDir/binary, ", OTP root(config):",
                    OtpPathConfig/binary>>
        end,
    Message =
        <<"Erlang LS (in ", Root/binary, "), version: ", BinVersion/binary, ", config from ",
            ConfigPath/binary, OtpMessage/binary>>,
    els_server:send_notification(
        <<"window/showMessage">>,
        #{
            type => ?MESSAGE_TYPE_INFO,
            message => Message
        }
    ),
    [];
execute_command(<<"ct-run-test">>, [Params]) ->
    els_command_ct_run_test:execute(Params),
    [];
execute_command(<<"function-references">>, [_Params]) ->
    [];
execute_command(<<"show-behaviour-usages">>, [_Params]) ->
    [];
execute_command(<<"suggest-spec">>, []) ->
    [];
execute_command(<<"suggest-spec">>, [
    #{
        <<"uri">> := Uri,
        <<"line">> := Line,
        <<"spec">> := Spec
    }
]) ->
    Method = <<"workspace/applyEdit">>,
    {ok, #{text := Text}} = els_utils:lookup_document(Uri),
    LineText = els_text:line(Text, Line - 1),
    NewText = <<Spec/binary, "\n", LineText/binary, "\n">>,
    Params =
        #{
            edit =>
                els_text_edit:edit_replace_text(Uri, NewText, Line - 1, Line)
        },
    els_server:send_request(Method, Params),
    [];
execute_command(<<"refactor.extract">>, [
    #{
        <<"uri">> := Uri,
        <<"range">> := Range
    }
]) ->
    ok = extract_function(Uri, Range),
    [];
execute_command(<<"bump-variables">>, [
    #{
        <<"uri">> := Uri,
        <<"range">> := Range,
        <<"name">> := Name
    }
]) ->
    ok = bump_variables(Uri, Range, Name),
    [];
execute_command(<<"add-behaviour-callbacks">>, [
    #{
        <<"uri">> := Uri,
        <<"behaviour">> := Behaviour
    }
]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    case els_utils:find_module(binary_to_atom(Behaviour, utf8)) of
        {error, _} ->
            [];
        {ok, BeUri} ->
            %% Put exported callback functions after -behaviour() or -export()
            #{range := #{to := {ExportLine, _Col}}} =
                lists:last(
                    els_poi:sort(
                        els_dt_document:pois(
                            Document,
                            [behaviour, export]
                        )
                    )
                ),
            ExportPos = {ExportLine + 1, 1},

            %% Put callback functions after the last function
            CallbacksPos =
                case els_poi:sort(els_dt_document:pois(Document, [function])) of
                    [] ->
                        {ExportLine + 2, 1};
                    POIs ->
                        #{data := #{wrapping_range := #{to := Pos}}} = lists:last(POIs),
                        Pos
                end,
            {ok, BeDoc} = els_utils:lookup_document(BeUri),
            CallbackPOIs = els_poi:sort(els_dt_document:pois(BeDoc, [callback])),
            FunPOIs = els_dt_document:pois(Document, [function]),

            %% Only add missing callback functions, existing functions are kept.
            Funs = [Id || #{id := Id} <- FunPOIs],
            Callbacks = [
                Cb
             || #{id := Id} = Cb <- CallbackPOIs,
                not lists:member(Id, Funs)
            ],
            Comment = ["\n%%% ", Behaviour, " callbacks\n"],
            ExportText = Comment ++ [export_text(Id) || Id <- Callbacks],
            Text = Comment ++ [fun_text(Cb, BeDoc) || Cb <- Callbacks],
            Method = <<"workspace/applyEdit">>,
            Params =
                #{
                    edit =>
                        #{
                            changes => #{
                                Uri =>
                                    [
                                        #{
                                            newText => iolist_to_binary(ExportText),
                                            range => els_protocol:range(
                                                #{
                                                    from => ExportPos,
                                                    to => ExportPos
                                                }
                                            )
                                        },
                                        #{
                                            newText => iolist_to_binary(Text),
                                            range => els_protocol:range(
                                                #{
                                                    from => CallbacksPos,
                                                    to => CallbacksPos
                                                }
                                            )
                                        }
                                    ]
                            }
                        }
                },
            els_server:send_request(Method, Params),
            []
    end;
execute_command(Command, Arguments) ->
    case wrangler_handler:execute_command(Command, Arguments) of
        true ->
            ok;
        _ ->
            ?LOG_INFO(
                "Unsupported command: [Command=~p] [Arguments=~p]",
                [Command, Arguments]
            )
    end,
    [].

-spec bump_variables(uri(), range(), binary()) -> ok.
bump_variables(Uri, Range, VarName) ->
    {Name, Number} = split_variable(VarName),
    {ok, Document} = els_utils:lookup_document(Uri),
    VarPOIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
    VarRange = els_range:to_poi_range(Range),
    ScopeRange = els_scope:variable_scope_range(VarRange, Document),
    Changes =
        [
            bump_variable_change(POI)
         || POI <- pois_in(VarPOIs, ScopeRange),
            should_bump_variable(POI, Name, Number)
        ],
    Method = <<"workspace/applyEdit">>,
    Params = #{edit => #{changes => #{Uri => Changes}}},
    els_server:send_request(Method, Params).

-spec should_bump_variable(els_poi:poi(), binary(), binary()) -> boolean().
should_bump_variable(#{id := Id}, Name, Number) ->
    case split_variable(Id) of
        {PName, PNumber} when PName == Name ->
            binary_to_integer(PNumber) >= binary_to_integer(Number);
        _ ->
            false
    end.

-spec bump_variable_change(els_poi:poi()) -> map().
bump_variable_change(#{id := Id, range := PoiRange}) ->
    {Name, Number} = split_variable(Id),
    NewNumber = integer_to_binary(binary_to_integer(Number) + 1),
    NewId = binary_to_atom(<<Name/binary, NewNumber/binary>>, utf8),
    #{
        newText => NewId,
        range => els_protocol:range(PoiRange)
    }.

-spec pois_in([els_poi:poi()], els_poi:poi_range()) ->
    [els_poi:poi()].
pois_in(POIs, Range) ->
    [POI || #{range := R} = POI <- POIs, els_range:in(R, Range)].

-spec split_variable(atom() | binary() | list()) -> {binary(), binary()} | error.
split_variable(Name) when is_atom(Name) ->
    split_variable(atom_to_list(Name));
split_variable(Name) when is_binary(Name) ->
    split_variable(unicode:characters_to_list(Name));
split_variable(Name) when is_list(Name) ->
    split_variable(lists:reverse(Name), []).

-spec split_variable(string(), string()) -> {binary(), binary()} | error.
split_variable([H | T], Acc) when $0 =< H, H =< $9 ->
    split_variable(T, [H | Acc]);
split_variable(_Name, []) ->
    error;
split_variable(Name, Acc) ->
    {list_to_binary(lists:reverse(Name)), list_to_binary(Acc)}.

-spec extract_function(uri(), range()) -> ok.
extract_function(Uri, Range) ->
    {ok, [#{text := Text} = Document]} = els_dt_document:lookup(Uri),
    ExtractRange = extract_range(Document, Range),
    #{from := {FromL, FromC} = From, to := {ToL, ToC}} = ExtractRange,
    ExtractString0 = els_text:range(Text, From, {ToL, ToC}),
    %% Trim whitespace
    ExtractString = string:trim(ExtractString0, both, " \n\r\t"),
    %% Trim trailing termination symbol
    ExtractStringTrimmed = string:trim(ExtractString, trailing, ",.;"),
    Method = <<"workspace/applyEdit">>,
    case els_dt_document:wrapping_functions(Document, FromL, FromC) of
        [WrappingFunPOI | _] when ExtractStringTrimmed /= <<>> ->
            %% WrappingFunPOI is the function that we are currently in
            #{
                data := #{
                    wrapping_range :=
                        #{
                            from := {FunBeginLine, _},
                            to := {FunEndLine, _}
                        }
                }
            } = WrappingFunPOI,
            %% Get args needed for the new function
            Args = get_args(ExtractRange, Document, FromL, FunBeginLine),
            ArgsBin = unicode:characters_to_binary(string:join(Args, ", ")),
            FunClause = <<"new_function(", ArgsBin/binary, ")">>,
            %% Place the new function after the current function
            EndSymbol = end_symbol(ExtractString),
            NewRange = els_protocol:range(
                #{from => {FunEndLine + 1, 1}, to => {FunEndLine + 1, 1}}
            ),
            FunBody = unicode:characters_to_list(
                <<FunClause/binary, " ->\n", ExtractStringTrimmed/binary, ".">>
            ),
            {ok, FunBodyFormatted, _} = erlfmt:format_string(FunBody, []),
            NewFun = unicode:characters_to_binary(FunBodyFormatted ++ "\n"),
            Changes = [
                #{
                    newText => <<FunClause/binary, EndSymbol/binary>>,
                    range => els_protocol:range(ExtractRange)
                },
                #{
                    newText => NewFun,
                    range => NewRange
                }
            ],
            Params = #{edit => #{changes => #{Uri => Changes}}},
            els_server:send_request(Method, Params);
        _ ->
            ?LOG_INFO("No wrapping function found"),
            ok
    end.

-spec end_symbol(binary()) -> binary().
end_symbol(ExtractString) ->
    case binary:last(ExtractString) of
        $. -> <<".">>;
        $, -> <<",">>;
        $; -> <<";">>;
        _ -> <<>>
    end.

%% @doc Find all variables defined in the function before the current.
%%      If they are used inside the selected range, they need to be
%%      sent in as arguments to the new function.
-spec get_args(
    els_poi:poi_range(),
    els_dt_document:item(),
    non_neg_integer(),
    non_neg_integer()
) -> [string()].
get_args(PoiRange, Document, FromL, FunBeginLine) ->
    %% TODO: Possible improvement. To make this bullet proof we should
    %% ignore vars defined inside LCs and funs()
    VarPOIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
    BeforeRange = #{from => {FunBeginLine, 1}, to => {FromL, 1}},
    VarsBefore = ids_in_range(BeforeRange, VarPOIs),
    VarsInside = ids_in_range(PoiRange, VarPOIs),
    els_utils:uniq([
        atom_to_list(Id)
     || Id <- VarsInside,
        lists:member(Id, VarsBefore)
    ]).

-spec ids_in_range(els_poi:poi_range(), [els_poi:poi()]) -> [atom()].
ids_in_range(PoiRange, VarPOIs) ->
    [
        Id
     || #{range := R, id := Id} <- VarPOIs,
        els_range:in(R, PoiRange)
    ].

-spec extract_range(els_dt_document:item(), range()) -> els_poi:poi_range().
extract_range(#{text := Text} = Document, Range) ->
    PoiRange = els_range:to_poi_range(Range),
    #{from := {CurrL, CurrC} = From, to := To} = PoiRange,
    POIs = els_dt_document:get_element_at_pos(Document, CurrL, CurrC),
    MarkedText = els_text:range(Text, From, To),
    case is_keyword_expr(MarkedText) of
        true ->
            case sort_by_range_size([P || #{kind := keyword_expr} = P <- POIs]) of
                [] ->
                    PoiRange;
                [{_Size, #{range := SmallestRange}} | _] ->
                    SmallestRange
            end;
        false ->
            PoiRange
    end.

-spec is_keyword_expr(binary()) -> boolean().
is_keyword_expr(Text) ->
    lists:member(Text, [
        <<"begin">>,
        <<"case">>,
        <<"fun">>,
        <<"if">>,
        <<"maybe">>,
        <<"receive">>,
        <<"try">>
    ]).

-spec sort_by_range_size(_) -> _.
sort_by_range_size(POIs) ->
    lists:sort([{range_size(P), P} || P <- POIs]).

-spec range_size(_) -> _.
range_size(#{range := #{from := {FromL, FromC}, to := {ToL, ToC}}}) ->
    {ToL - FromL, ToC - FromC}.

-spec spec_text(binary()) -> binary().
spec_text(<<"-callback", Rest/binary>>) ->
    <<"-spec", Rest/binary>>;
spec_text(Text) ->
    Text.

-spec fun_text(els_poi:poi(), els_dt_document:item()) -> iolist().
fun_text(#{id := {Name, Arity}, range := Range}, #{text := Text}) ->
    #{from := From, to := To} = Range,
    %% TODO: Assuming 2 space indentation
    CallbackText = els_text:range(Text, From, To),
    SpecText = spec_text(CallbackText),
    [
        io_lib:format("~s", [SpecText]),
        "\n",
        atom_to_binary(Name, utf8),
        "(",
        args_text(Arity, 1),
        ") ->\n",
        "  error(not_implemented).\n\n"
    ].

-spec export_text(els_poi:poi()) -> iolist().
export_text(#{id := {Name, Arity}}) ->
    [
        "-export([",
        atom_to_binary(Name, utf8),
        "/",
        integer_to_list(Arity),
        "]).\n"
    ].

-spec args_text(integer(), integer()) -> iolist().
args_text(0, 1) ->
    [];
args_text(Arity, Arity) ->
    ["_"];
args_text(Arity, N) ->
    ["_, " | args_text(Arity, N + 1)].
