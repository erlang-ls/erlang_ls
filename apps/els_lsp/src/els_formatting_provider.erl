-module(els_formatting_provider).

-behaviour(els_provider).

-export([ handle_request/2
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
-type state() :: any().

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(DEFAULT_SUB_INDENT, 2).

%%==============================================================================
%% els_provider functions
%%==============================================================================

%% Keep the behaviour happy
-spec is_enabled() -> boolean().
is_enabled() -> is_enabled_document().

-spec is_enabled_document() -> boolean().
is_enabled_document() -> true.

-spec is_enabled_range() -> boolean().
is_enabled_range() ->
  true.

%% NOTE: because erlang_ls does not send incremental document changes
%%       via `textDocument/didChange`, this kind of formatting does not
%%       make sense.
-spec is_enabled_on_type() -> document_ontypeformatting_options().
is_enabled_on_type() ->
  case els_config:get(format_on_type) of
    true -> #{firstTriggerCharacter => <<".">>, moreTriggerCharacter => []};
    false -> false
  end.

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
      case els_config:get(bsp_enabled) of
        true ->
          {format_document_bsp(Path, RelativePath, Options), State};
        false ->
          {format_document_local(Path, RelativePath, Options), State}
      end
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

-spec format_document_bsp(binary(), string(), formatting_options()) ->
        [text_edit()].
format_document_bsp(Path, RelativePath, _Options) ->
  Fun = fun(Dir) ->
            Params = #{ <<"output">>  => els_utils:to_binary(Dir)
                      , <<"file">> => els_utils:to_binary(RelativePath)
                      },
            els_bsp_client:request(<<"custom/format">>, Params),
            OutFile = filename:join(Dir, RelativePath),
            els_text_edit:diff_files(Path, OutFile)
        end,
  tempdir:mktmp(Fun).

-spec format_document_local(binary(), string(), formatting_options()) ->
        [text_edit()].
format_document_local(Path, RelativePath,
                      #{ <<"insertSpaces">> := InsertSpaces
                       , <<"tabSize">> := TabSize } = Options) ->
  SubIndent = maps:get(<<"subIndent">>, Options, ?DEFAULT_SUB_INDENT),
  Opts0 = #{ remove_tabs => InsertSpaces
           , break_indent => TabSize
           , sub_indent => SubIndent
           },
  Fun = fun(Dir) ->
            Opts = Opts0#{output_dir => Dir},
            Formatter = rebar3_formatter:new(default_formatter, Opts, unused),
            rebar3_formatter:format_file(RelativePath, Formatter),
            OutFile = filename:join(Dir, RelativePath),
            els_text_edit:diff_files(Path, OutFile)
        end,
  tempdir:mktmp(Fun).

-spec rangeformat_document(uri(), map(), range(), formatting_options())
                          -> {ok, [text_edit()]}.
rangeformat_document(_Uri, _Document, _Range, _Options) ->
    {ok, []}.

-spec ontypeformat_document(binary(), map()
                           , number(), number(), string(), formatting_options())
                           -> {ok, [text_edit()]}.
ontypeformat_document(_Uri, Document, Line, Col, <<".">>, _Options) ->
  case find_matching_range(Document, Line) of
    [] ->
      {ok, []};
    [MatchingRange] ->
      {StartLine, _} = Id = els_poi:id(MatchingRange),
      Text = els_dt_document:text(Document),
      RangeText = els_text:range(Text, Id, {Line, Col}),
      % Skip formatting if the . is on a commented line.
      case string:trim(els_text:line(Text, Line - 1), both) of
        <<"%", _/binary>> ->
          {ok, []};
        _ ->
          ParseF =
            fun(Dir) ->
               TmpFile = tmp_file(Dir),
               ok = file:write_file(TmpFile, RangeText),
               Opts = #{break_indent => 2, output_dir => current},
               RebarState = #{},
               T = rebar3_formatter:new(default_formatter, Opts, RebarState),
               rebar3_formatter:format_file(TmpFile, T),
               {ok, Bin} = file:read_file(TmpFile),
               Bin
            end,
          %% rebar3_formatter adds a newline, since we terminate on .
          %% We want to leave the cursor at the current char rather
          %% than jumping to a newline
          NewText =
            string:trim(
              tempdir:mktmp(ParseF), trailing, "\n"),
          {ok,
           [#{range =>
                #{start => #{line => StartLine - 1, character => 0},
                  'end' => #{line => Line - 1, character => Col}},
              newText => NewText}]}
      end
  end;
ontypeformat_document(_Uri, _Document, _Line, _Col, Char, _Options) ->
  ?LOG_INFO("Got unhandled character in ontypeformat_document. No formatter "
            "configured for char: ~p",
            [Char]),
  {ok, []}.

-spec find_foldable_ranges(els_dt_document:item()) -> [poi()].
find_foldable_ranges(Document) ->
  Pois = els_dt_document:pois(Document),
  lists:filter(fun (#{kind := folding_range}) ->
                     true;
                   (_) ->
                     false
               end,
               Pois).

-spec find_matching_range(els_dt_document:item(), number()) -> [poi()].
find_matching_range(Document, Line) ->
  lists:filter(fun(#{range := #{from := {FromLine, _}, to := {ToLine, _}}}) ->
                  Line >= FromLine andalso Line =< ToLine
               end,
               find_foldable_ranges(Document)).

-spec tmp_file(string()) -> any().
tmp_file(Dir) ->
  Unique = erlang:unique_integer([positive]),
  {A, B, C} = os:timestamp(),
  N = node(),
  filename:join(Dir,
                lists:flatten(
                  io_lib:format("~p-~p.~p.~p.~p", [N, A, B, C, Unique]))).
