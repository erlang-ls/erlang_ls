-module(els_formatting_provider).

-behaviour(els_provider).

-export([
    init/0,
    handle_request/2,
    is_enabled/0,
    is_enabled_document/0,
    is_enabled_range/0,
    is_enabled_on_type/0
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type formatter() :: fun((string(), string(), formatting_options()) -> boolean()).
-type state() :: [formatter()].

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(DEFAULT_SUB_INDENT, 2).

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec init() -> state().
init() ->
    [fun format_document_local/3].

%% Keep the behaviour happy
-spec is_enabled() -> boolean().
is_enabled() -> is_enabled_document().

-spec is_enabled_document() -> boolean().
is_enabled_document() -> true.

-spec is_enabled_range() -> boolean().
is_enabled_range() ->
    false.

%% NOTE: because erlang_ls does not send incremental document changes
%%       via `textDocument/didChange`, this kind of formatting does not
%%       make sense.
-spec is_enabled_on_type() -> document_ontypeformatting_options().
is_enabled_on_type() -> false.

-spec handle_request(any(), state()) -> {response, any()}.
handle_request({document_formatting, Params}, _State) ->
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
handle_request({document_rangeformatting, Params}, _State) ->
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
handle_request({document_ontypeformatting, Params}, _State) ->
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
    Formatter = rebar3_formatter:new(default_formatter, Opts, unused),
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
