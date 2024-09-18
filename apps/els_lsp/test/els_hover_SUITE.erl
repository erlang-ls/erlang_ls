-module(els_hover_SUITE).

-include("els_lsp.hrl").

%% CT Callbacks
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([
    local_call_no_args/1,
    local_call_with_args/1,
    remote_call_multiple_clauses/1,
    local_call_edoc/1,
    remote_call_edoc/1,
    remote_call_otp/1,
    local_fun_expression/1,
    remote_fun_expression/1,
    no_poi/1,
    included_macro/1,
    edoc_spec/1,
    edoc_definition/1,
    local_macro/1,
    weird_macro/1,
    macro_with_zero_args/1,
    macro_with_args/1,
    local_record/1,
    included_record/1,
    local_type/1,
    local_type_definition/1,
    remote_type/1,
    local_opaque/1,
    local_opaque_definition/1,
    remote_opaque/1,
    nonexisting_type/1,
    nonexisting_module/1,
    memoize/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec suite() -> [tuple()].
suite() ->
    [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
    els_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) when
    TestCase == local_call_with_args;
    TestCase == local_fun_expression;
    TestCase == local_record
->
    case els_utils:is_windows() of
        true ->
            %% TODO: Testcase fails on windows since OTP 24, fix!
            {skip, "Testcase not supported on Windows."};
        false ->
            els_test_utils:init_per_testcase(TestCase, Config)
    end;
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
local_call_no_args(Config) ->
    %% this test is for the fallback render when no doc chunks are available
    CleanupMock = mock_doc_chunks_unavailable(),
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 10, 7),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value = <<"## local_call/0">>,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    CleanupMock(),
    ok.

local_call_with_args(Config) ->
    %% this test is for the fallback render when no doc chunks are available
    CleanupMock = mock_doc_chunks_unavailable(),
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 13, 7),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value = <<
        "## local_call/2\n\n"
        "---\n\n"
        "```erlang\n\n"
        "  local_call(Arg1, Arg2) \n\n"
        "```\n\n"
        "```erlang\n"
        "-spec local_call(integer(), any()) -> tuple();\n"
        "                (float(), any()) -> tuple().\n"
        "```"
    >>,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    CleanupMock(),
    ok.

remote_call_multiple_clauses(Config) ->
    %% this test is for the fallback render when no doc chunks are available
    CleanupMock = mock_doc_chunks_unavailable(),
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 16, 15),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value = <<
        "## hover_docs:multiple_clauses/1\n\n"
        "---\n\n"
        "```erlang\n\n"
        "  multiple_clauses(L) when is_list(L)\n\n"
        "  multiple_clauses(#{data := Data}) \n\n"
        "  multiple_clauses(X) \n\n```"
    >>,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    CleanupMock(),
    ok.

local_call_edoc(Config) ->
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 29, 5),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value =
        case has_eep48_edoc() of
            true ->
                <<"```erlang\nedoc() -> ok.\n```\n\n---\n\nAn edoc hover item\n">>;
            false ->
                <<
                    "## edoc/0\n\n---\n\n```erlang\n-spec edoc() -> ok.\n```\n\n"
                    "An edoc hover item\n\n"
                >>
        end,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    ok.

remote_call_edoc(Config) ->
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 23, 12),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value =
        case has_eep48_edoc() of
            true ->
                <<"```erlang\nedoc() -> ok.\n```\n\n---\n\nAn edoc hover item\n">>;
            false ->
                <<
                    "## hover_docs:edoc/0\n\n---\n\n```erlang\n-spec edoc() -> ok.\n"
                    "```\n\nAn edoc hover item\n\n"
                >>
        end,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    ok.

remote_call_otp(Config) ->
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 26, 12),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    Value =
        case has_eep48(file) of
            true when OtpRelease >= 27 ->
                <<
                    "## file:write/2\n\n---\n\n"
                    "```erlang\n\n  write(File, Bytes) when is_pid(File) orelse is_atom(File)\n\n"
                    "  write(#file_descriptor{module = Module} = Handle, Bytes) \n\n"
                    "  write(_, _) \n\n"
                    "```\n\n"
                    "```erlang\n"
                    "-spec write(IoDevice, Bytes) -> ok | {error, Reason} when\n"
                    "      IoDevice :: io_device() | io:device(),\n"
                    "      Bytes :: iodata(),\n"
                    "      Reason :: posix() | badarg | terminated.\n"
                    "```"
                >>;
            true when OtpRelease == 26 ->
                <<
                    "```erlang\nwrite(IoDevice, Bytes) -> ok | {error, "
                    "Reason}\nwhen\n  IoDevice :: io_device() | io:device(),\n"
                    "  Bytes :: iodata(),"
                    "\n  Reason :: posix() | badarg | terminated.\n```\n\n"
                    "---\n\n"
                    "Writes `Bytes` to the file referenced by `IoDevice`\\."
                    " This function is the only way to write to a file opened in"
                    " `raw` mode \\(although it works for normally opened files"
                    " too\\)\\. Returns `ok` if successful, and"
                    " `{error, Reason}` otherwise\\.\n\nIf the file is opened"
                    " with `encoding` set to something else than `latin1`,"
                    " each byte written can result in many bytes being written"
                    " to the file, as the byte range 0\\.\\.255 can represent"
                    " anything between one and four bytes depending on value"
                    " and UTF encoding type\\. If you want to write"
                    " [`unicode:chardata()`](https://erlang.org/doc/man/unicode"
                    ".html#type-chardata) to the `IoDevice` you should use"
                    " [`io:put_chars/2`](https://erlang.org/doc/man/io.html"
                    "#put_chars-2) instead\\.\n\nTypical error reasons:\n\n"
                    "* **`ebadf`**  \n  The file is not opened for writing\\.\n\n"
                    "* **`enospc`**  \n  No space is left on the device\\.\n"
                >>;
            true ->
                <<
                    "```erlang\nwrite(IoDevice, Bytes) -> ok | {error, Reason}\n"
                    "when\n  IoDevice :: io_device() | atom(),\n  Bytes :: iodata(),"
                    "\n  Reason :: posix() | badarg | terminated.\n```\n\n---\n\n"
                    "Writes `Bytes` to the file referenced by `IoDevice`\\. This "
                    "function is the only way to write to a file opened in `raw` "
                    "mode \\(although it works for normally opened files too\\)\\. "
                    "Returns `ok` if successful, and `{error, Reason}` otherwise\\."
                    "\n\nIf the file is opened with `encoding` set to something else "
                    "than `latin1`, each byte written can result in many bytes being "
                    "written to the file, as the byte range 0\\.\\.255 can represent "
                    "anything between one and four bytes depending on value and UTF "
                    "encoding type\\.\n\nTypical error reasons:\n\n* **`ebadf`**  \n"
                    "  The file is not opened for writing\\.\n\n* **`enospc`**  \n"
                    "  No space is left on the device\\.\n"
                >>;
            false ->
                <<
                    "## file:write/2\n\n---\n\n```erlang\n\n  write(File, Bytes) "
                    "when is_pid(File) orelse is_atom(File)\n\n  write(#file_"
                    "descriptor{module = Module} = Handle, Bytes) \n\n  "
                    "write(_, _) \n\n```\n\n```erlang\n-spec write(IoDevice, Bytes)"
                    " -> ok | {error, Reason} when\n      IoDevice :: io_device() |"
                    " atom(),\n      Bytes :: iodata(),\n      Reason :: posix() | "
                    "badarg | terminated.\n```"
                >>
        end,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    ok.

local_fun_expression(Config) ->
    %% this test is for the fallback render when no doc chunks are available
    CleanupMock = mock_doc_chunks_unavailable(),
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 19, 5),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value = <<
        "## local_call/2\n\n"
        "---\n\n"
        "```erlang\n\n"
        "  local_call(Arg1, Arg2) \n\n"
        "```\n\n"
        "```erlang\n"
        "-spec local_call(integer(), any()) -> tuple();\n"
        "                (float(), any()) -> tuple().\n"
        "```"
    >>,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    CleanupMock(),
    ok.

remote_fun_expression(Config) ->
    %% this test is for the fallback render when no doc chunks are available
    CleanupMock = mock_doc_chunks_unavailable(),
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 20, 10),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value = <<
        "## hover_docs:multiple_clauses/1\n\n"
        "---\n\n"
        "```erlang\n\n"
        "  multiple_clauses(L) when is_list(L)\n\n"
        "  multiple_clauses(#{data := Data}) \n\n"
        "  multiple_clauses(X) \n\n```"
    >>,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    CleanupMock(),
    ok.

edoc_definition(Config) ->
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 41, 3),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value =
        case has_eep48_edoc() of
            true ->
                <<"```erlang\nedoc() -> ok.\n```\n\n---\n\nAn edoc hover item\n">>;
            false ->
                <<
                    "## edoc/0\n\n---\n\n```erlang\n-spec edoc() -> ok.\n```\n\n"
                    "An edoc hover item\n\n"
                >>
        end,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    ok.

edoc_spec(Config) ->
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 40, 10),
    ?assert(maps:is_key(contents, Result)),
    Contents = maps:get(contents, Result),
    Value =
        case has_eep48_edoc() of
            true ->
                <<"```erlang\nedoc() -> ok.\n```\n\n---\n\nAn edoc hover item\n">>;
            false ->
                <<
                    "## edoc/0\n\n---\n\n```erlang\n-spec edoc() -> ok.\n```\n\n"
                    "An edoc hover item\n\n"
                >>
        end,
    Expected = #{
        kind => <<"markdown">>,
        value => Value
    },
    ?assertEqual(Expected, Contents),
    ok.

local_macro(Config) ->
    Uri = ?config(hover_macro_uri, Config),
    #{result := Result} = els_client:hover(Uri, 6, 4),
    Value = <<"```erlang\n?LOCAL_MACRO = local_macro\n```">>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

included_macro(Config) ->
    Uri = ?config(hover_macro_uri, Config),
    #{result := Result} = els_client:hover(Uri, 7, 4),
    Value = <<"```erlang\n?INCLUDED_MACRO_A = included_macro_a\n```">>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

weird_macro(Config) ->
    Uri = ?config(hover_macro_uri, Config),
    #{result := Result} = els_client:hover(Uri, 12, 20),
    Value = <<"```erlang\n?WEIRD_MACRO = A when A > 1\n```">>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

macro_with_zero_args(Config) ->
    Uri = ?config(hover_macro_uri, Config),
    #{result := Result} = els_client:hover(Uri, 18, 10),
    Value = <<"```erlang\n?MACRO_WITH_ARGS() = {macro}\n```">>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

macro_with_args(Config) ->
    Uri = ?config(hover_macro_uri, Config),
    #{result := Result} = els_client:hover(Uri, 19, 10),
    Value = <<"```erlang\n?MACRO_WITH_ARGS(X, Y) = {macro, X, Y}\n```">>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

no_poi(Config) ->
    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Result} = els_client:hover(Uri, 10, 1),
    ?assertEqual(null, Result),
    ok.

local_record(Config) ->
    Uri = ?config(hover_record_expr_uri, Config),
    #{result := Result} = els_client:hover(Uri, 11, 4),
    Value = <<
        "```erlang\n-record(test_record, {\n"
        "        field1 = 123,\n"
        "        field2 = xyzzy,\n"
        "        field3\n"
        "}).\n```"
    >>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

included_record(Config) ->
    Uri = ?config(hover_record_expr_uri, Config),
    #{result := Result} = els_client:hover(Uri, 15, 4),
    Value = <<
        "```erlang\n"
        "-record(included_record_a, {included_field_a, included_field_b})."
        "\n```"
    >>,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

local_type(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 6, 10),
    Value =
        case has_eep48_edoc() of
            true -> <<"```erlang\n-type type_a() :: any().\n```\n\n---\n\n\n">>;
            false -> <<"```erlang\n-type type_a() :: any().\n```">>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

local_type_definition(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 3, 8),
    Value =
        case has_eep48_edoc() of
            true -> <<"```erlang\n-type type_a() :: any().\n```\n\n---\n\n\n">>;
            false -> <<"```erlang\n-type type_a() :: any().\n```">>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

remote_type(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 10, 10),
    Value =
        case has_eep48_edoc() of
            true -> <<"```erlang\n-type type_a() :: atom().\n```\n\n---\n\n\n">>;
            false -> <<"```erlang\n-type type_a() :: atom().\n```">>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

local_opaque(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 14, 10),
    Value =
        case has_eep48_edoc() of
            true -> <<"```erlang\n-opaque opaque_type_a() \n```\n\n---\n\n\n">>;
            false -> <<"```erlang\n-opaque opaque_type_a() :: any().\n```">>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

local_opaque_definition(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 4, 11),
    Value =
        case has_eep48_edoc() of
            true -> <<"```erlang\n-opaque opaque_type_a() \n```\n\n---\n\n\n">>;
            false -> <<"```erlang\n-opaque opaque_type_a() :: any().\n```">>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

remote_opaque(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 18, 10),
    Value =
        case has_eep48_edoc() of
            true -> <<"```erlang\n-opaque opaque_type_a() \n```\n\n---\n\n\n">>;
            false -> <<"```erlang\n-opaque opaque_type_a() :: atom().\n```">>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

nonexisting_type(Config) ->
    Uri = ?config(hover_type_uri, Config),
    #{result := Result} = els_client:hover(Uri, 22, 15),
    %% The spec for `j' is shown instead of the type docs.
    Value =
        case list_to_integer(erlang:system_info(otp_release)) >= 25 of
            true ->
                <<
                    "```erlang\nj(_ :: doesnt:exist()) -> ok.\n```\n\n"
                    "---\n\n\n"
                >>;
            % els_eep48_docs:render returns {error, function_missing} for
            % OTP versions under 25
            _ ->
                <<
                    "## j/1\n\n---\n\n```erlang\n\n  j(_) \n\n```\n\n"
                    "```erlang\n-spec j(doesnt:exist()) -> ok.\n```"
                >>
        end,
    Expected = #{
        contents => #{
            kind => <<"markdown">>,
            value => Value
        }
    },
    ?assertEqual(Expected, Result),
    ok.

nonexisting_module(Config) ->
    Uri = ?config(hover_nonexisting_uri, Config),
    #{result := Result} = els_client:hover(Uri, 6, 12),
    Expected = #{
        contents =>
            #{
                kind => <<"markdown">>,
                value => <<"## nonexisting:main/0">>
            }
    },
    ?assertEqual(Expected, Result),
    ok.

memoize(Config) ->
    RootUri = els_test_utils:root_uri(),
    DataDir = ?config(data_dir, Config),
    ConfigPath = filename:join(DataDir, "docs_memo_true.config"),
    InitOpts = #{<<"erlang">> => #{<<"config_path">> => ConfigPath}},
    els_client:initialize(RootUri, InitOpts),

    Uri = ?config(hover_docs_caller_uri, Config),
    #{result := Expected} = els_client:hover(Uri, 41, 3),
    {ok, [Item]} = els_docs_memo:lookup({hover_docs_caller, edoc, 0, 'local', function}),
    #{entries := Entries} = Item,

    %% JSON RPC
    Encoded = jsx:encode(#{contents => els_markup_content:new(Entries)}),
    Result = jsx:decode(Encoded, [{labels, atom}]),

    ?assertEqual(Expected, Result),
    ok.

%%==============================================================================
%% Helpers
%%==============================================================================

%% @doc
%% Returns a function that can be used to unload the mock.
%% @end
mock_doc_chunks_unavailable() ->
    meck:expect(
        els_docs,
        eep48_docs,
        fun(_, _, _, _) -> {error, not_available} end
    ),
    fun() ->
        ok = meck:unload(els_docs)
    end.

has_eep48_edoc() ->
    list_to_integer(erlang:system_info(otp_release)) >= 24.

has_eep48(Module) ->
    case catch code:get_doc(Module) of
        {ok, {docs_v1, _, erlang, _, _, _, Docs}} ->
            lists:any(
                fun
                    ({_, _, _, Doc, _}) when is_map(Doc) -> true;
                    ({_, _, _, _, _}) -> false
                end,
                Docs
            );
        _ ->
            false
    end.
