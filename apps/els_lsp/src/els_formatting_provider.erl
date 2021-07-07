-module(els_formatting_provider).

-behaviour(els_provider).

-export([ init/0
        , handle_request/2
        , is_enabled/0
        , is_enabled_document/0
        , is_enabled_range/0
        , is_enabled_on_type/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type formatter() :: fun((string(), string(), formatting_options()) ->
                            boolean()).
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
  case els_config:get(bsp_enabled) of
    false ->
      [ fun format_document_local/3 ];
    _ ->
      [ fun format_document_bsp/3,  fun format_document_local/3 ]
  end.

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

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_formatting, Params}, State) ->
  #{ <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  Path = els_uri:path(Uri),
  case els_utils:project_relative(Uri) of
    {error, not_relative} ->
      {[], State};
    RelativePath ->
      format_document(Path, RelativePath, Options, State)
  end;
handle_request({document_rangeformatting, Params}, State) ->
  #{ <<"range">>     := #{ <<"start">> := StartPos
                         , <<"end">>   := EndPos
                         }
   , <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  Range = #{ start => StartPos, 'end' => EndPos },
  {ok, Document} = els_utils:lookup_document(Uri),
  case rangeformat_document(Uri, Document, Range, Options) of
    {ok, TextEdit} -> {TextEdit, State}
  end;
handle_request({document_ontypeformatting, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"ch">>           := Char
   , <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  case ontypeformat_document(Uri, Document, Line + 1, Character + 1, Char
                            , Options) of
    {ok, TextEdit} -> {TextEdit, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec format_document(binary(), string(), formatting_options(), state()) ->
        {[text_edit()], state()}.
format_document(Path, RelativePath, Options, Formatters) ->
  Fun = fun(Dir) ->
            NewFormatters = lists:dropwhile(
                              fun(F) ->
                                  not F(Dir, RelativePath, Options)
                              end,
                              Formatters
                             ),
            Outfile = filename:join(Dir, RelativePath),
            {els_text_edit:diff_files(Path, Outfile), NewFormatters}
        end,
  tempdir:mktmp(Fun).

-spec format_document_bsp(string(), string(), formatting_options()) ->
           boolean().
format_document_bsp(Dir, RelativePath, _Options) ->
  Method = <<"rebar3/run">>,
  Params = #{ <<"args">> => ["format", "-o", Dir, "-f", RelativePath] },
  try
    case els_bsp_provider:request(Method, Params) of
      {error, Reason} ->
        error(Reason);
      {reply, #{ error := _Error } = Result} ->
        error(Result);
      {reply, Result} ->
        ?LOG_DEBUG("BSP format succeeded. [result=~p]", [Result]),
        true
    end
  catch
    C:E:S ->
      ?LOG_WARNING("format_document_bsp failed. ~p:~p ~p", [C, E, S]),
      false
  end.

-spec format_document_local(string(), string(), formatting_options()) ->
           boolean().
format_document_local(Dir, RelativePath,
                      #{ <<"insertSpaces">> := InsertSpaces
                       , <<"tabSize">> := TabSize } = Options) ->
  SubIndent = maps:get(<<"subIndent">>, Options, ?DEFAULT_SUB_INDENT),
  Opts = #{ remove_tabs => InsertSpaces
          , break_indent => TabSize
          , sub_indent => SubIndent
          , output_dir => Dir
          },
  Formatter = rebar3_formatter:new(default_formatter, Opts, unused),
  rebar3_formatter:format_file(RelativePath, Formatter),
  true.

-spec rangeformat_document(uri(), map(), range(), formatting_options())
                          -> {ok, [text_edit()]}.
rangeformat_document(_Uri, _Document, _Range, _Options) ->
    {ok, []}.

-spec ontypeformat_document(binary(), map()
                           , number(), number(), string(), formatting_options())
                           -> {ok, [text_edit()]}.
ontypeformat_document(_Uri, _Document, _Line, _Col, _Char, _Options) ->
    {ok, []}.
