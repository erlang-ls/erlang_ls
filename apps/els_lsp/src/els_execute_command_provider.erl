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
        <<"add-behaviour-callbacks">>
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
