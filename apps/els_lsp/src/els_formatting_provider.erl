-module(els_formatting_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(DEFAULT_SUB_INDENT, 2).

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
    {[text_edit()]}.
format_document(Path, RelativePath, Options) ->
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
    SubIndent = maps:get(<<"subIndent">>, Options, ?DEFAULT_SUB_INDENT),
    Opts = #{
        remove_tabs => InsertSpaces,
        break_indent => TabSize,
        sub_indent => SubIndent,
        output_dir => Dir
    },
    Config = els_config:get(formatting),
    FormatterName = get_formatter_name(Config),
    Formatter = rebar3_formatter:new(FormatterName, Opts, unused),
    rebar3_formatter:format_file(RelativePath, Formatter),
    ok.

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

-spec get_formatter_name(map() | undefined) ->
    sr_formatter | erlfmt_formatter | otp_formatter | default_formatter.
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
