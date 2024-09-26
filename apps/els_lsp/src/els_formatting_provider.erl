-module(els_formatting_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(DEFAULT_SUB_INDENT, 2).
-type formatter_name() ::
    sr_formatter
    | erlfmt_formatter
    | otp_formatter
    | default_formatter.

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {response, any()}.
handle_request({document_formatting, Params}) ->
    #{
        <<"options">> := Options,
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    Path = els_uri:path(Uri),
    case els_utils:project_relative(Uri) of
        {error, not_relative} ->
            {response, []};
        RelativePath ->
            format_document(Path, RelativePath, Options)
    end;
handle_request({document_rangeformatting, Params}) ->
    #{
        <<"range">> := #{
            <<"start">> := StartPos,
            <<"end">> := EndPos
        },
        <<"options">> := Options,
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    Range = #{start => StartPos, 'end' => EndPos},
    {ok, Document} = els_utils:lookup_document(Uri),
    {ok, TextEdit} = rangeformat_document(Uri, Document, Range, Options),
    {response, TextEdit};
%% NOTE: because erlang_ls does not send incremental document changes
%%       via `textDocument/didChange`, this kind of formatting does not
%%       make sense.
handle_request({document_ontypeformatting, Params}) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"ch">> := Char,
        <<"options">> := Options,
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    {ok, Document} = els_utils:lookup_document(Uri),
    {ok, TextEdit} =
        ontypeformat_document(
            Uri,
            Document,
            Line + 1,
            Character + 1,
            Char,
            Options
        ),
    {response, TextEdit}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec format_document(binary(), string(), formatting_options()) ->
    {response, [text_edit()]}.
format_document(Path, RelativePath, Options) ->
    Config = els_config:get(formatting),
    case {get_formatter_files(Config), get_formatter_exclude_files(Config)} of
        {all, ExcludeFiles} ->
            case lists:member(Path, ExcludeFiles) of
                true ->
                    {response, []};
                false ->
                    do_format_document(Path, RelativePath, Options)
            end;
        {Files, ExcludeFiles} ->
            case lists:member(Path, Files) of
                true ->
                    case lists:member(Path, ExcludeFiles) of
                        true ->
                            {response, []};
                        false ->
                            do_format_document(Path, RelativePath, Options)
                    end;
                false ->
                    {response, []}
            end
    end.

-spec do_format_document(binary(), string(), formatting_options()) ->
    {response, [text_edit()]}.
do_format_document(Path, RelativePath, Options) ->
    Fun = fun(Dir) ->
        format_document_local(Dir, RelativePath, Options),
        Outfile = filename:join(Dir, RelativePath),
        {response, els_text_edit:diff_files(Path, Outfile)}
    end,
    tempdir:mktmp(Fun).

-spec format_document_local(string(), string(), formatting_options()) -> ok.
format_document_local(
    Dir,
    RelativePath,
    #{
        <<"insertSpaces">> := InsertSpaces,
        <<"tabSize">> := TabSize
    } = Options
) ->
    SubIndent = get_sub_indent(Options),
    Opts0 = #{
        remove_tabs => InsertSpaces,
        break_indent => TabSize,
        sub_indent => SubIndent,
        output_dir => Dir
    },
    Config = els_config:get(formatting),
    FormatterName = get_formatter_name(Config),
    Opts = maybe_set_width(FormatterName, Opts0, get_width(Config)),
    ?LOG_INFO("Format using ~p with options: ~p", [FormatterName, Opts]),
    Formatter = rebar3_formatter:new(FormatterName, Opts, unused),
    rebar3_formatter:format_file(RelativePath, Formatter),
    ok.

-spec get_sub_indent(map()) -> integer().
get_sub_indent(Options) ->
    maps:get("subIndent", Options, ?DEFAULT_SUB_INDENT).

-spec maybe_set_width(formatter_name(), map(), integer() | undefined) -> map().
maybe_set_width(erlfmt_formatter, Opts, Width) when is_integer(Width) ->
    Opts#{print_width => Width};
maybe_set_width(default_formatter, Opts, Width) when is_integer(Width) ->
    Opts#{paper => Width};
maybe_set_width(otp_formatter, Opts, Width) when is_integer(Width) ->
    Opts#{paper => Width};
maybe_set_width(_Formatter, Opts, _) ->
    Opts.

-spec get_width(map()) -> integer() | undefined.
get_width(Config) ->
    maps:get("width", Config, undefined).

-spec rangeformat_document(uri(), map(), range(), formatting_options()) ->
    {ok, [text_edit()]}.
rangeformat_document(_Uri, _Document, _Range, _Options) ->
    {ok, []}.

-spec ontypeformat_document(
    binary(),
    map(),
    number(),
    number(),
    string(),
    formatting_options()
) ->
    {ok, [text_edit()]}.
ontypeformat_document(_Uri, _Document, _Line, _Col, _Char, _Options) ->
    {ok, []}.

-spec get_formatter_files(map()) -> [binary()] | all.
get_formatter_files(Config) ->
    RootPath = els_uri:path(els_config:get(root_uri)),
    case maps:get("files", Config, all) of
        all ->
            all;
        Globs ->
            lists:flatten([expand_glob(RootPath, Glob) || Glob <- Globs])
    end.

-spec get_formatter_exclude_files(map()) -> [binary()].
get_formatter_exclude_files(Config) ->
    RootPath = els_uri:path(els_config:get(root_uri)),
    Globs = maps:get("exclude_files", Config, []),
    lists:flatten([expand_glob(RootPath, Glob) || Glob <- Globs]).

-spec expand_glob(binary(), binary()) -> [binary()].
expand_glob(RootPath, Glob) ->
    Wildcard = unicode:characters_to_list(filename:join(RootPath, Glob), utf8),
    [unicode:characters_to_binary(Path) || Path <- filelib:wildcard(Wildcard)].

-spec get_formatter_name(map() | undefined) -> formatter_name().
get_formatter_name(undefined) ->
    default_formatter;
get_formatter_name(Config) ->
    case maps:get("formatter", Config, undefined) of
        "sr" ->
            sr_formatter;
        "erlfmt" ->
            erlfmt_formatter;
        "otp" ->
            otp_formatter;
        _ ->
            default_formatter
    end.
