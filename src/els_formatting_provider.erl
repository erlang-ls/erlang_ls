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
-include("erlang_ls.hrl").

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
  false.

%% NOTE: because erlang_ls does not send incremental document changes
%%       via `textDocument/didChange`, this kind of formatting does not
%%       make sense.
-spec is_enabled_on_type() -> document_ontypeformatting_options().
is_enabled_on_type() -> false.
%% is_enabled_on_type() -> #{ <<"firstTriggerCharacter">> => "\n"
%%                          , <<"moreTriggerCharacter">>  => ",;."
%%                          }.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({document_formatting, Params}, State) ->
  lager:info("els_formatting_provide:document_formatting: [Params=~p]"
            , [Params]),
  #{ <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  lager:info("els_formatting_provide:document_formatting: [Options=~p]"
            , [Options]),
  {ok, Document} = els_utils:find_document(Uri),
  case format_document(Uri, Document, Options) of
    {ok, TextEdit} -> {TextEdit, State}
  end;
handle_request({document_rangeformatting, Params}, State) ->
  #{ <<"range">>     := #{ <<"start">> := StartPos
                         , <<"end">>   := EndPos
                         }
   , <<"options">>      := Options
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  Range = #{ start => StartPos, 'end' => EndPos },
  {ok, Document} = els_utils:find_document(Uri),
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
  {ok, Document} = els_utils:find_document(Uri),
  case ontypeformat_document(Uri, Document, Line + 1, Character + 1, Char
                            , Options) of
    {ok, TextEdit} -> {TextEdit, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec format_document(uri(), els_document:document(), formatting_options())
                     -> {ok, [text_edit()]}.
format_document(Uri, _Document, Options) ->
    lager:info("format_document: ~p", [{Uri, Options}]),
    Path = els_uri:path(Uri),
    Fun = fun(Dir) ->
            RelPath = els_utils:project_relative(Uri),
            OutFile = filename:join(Dir, RelPath),
            Opts = #{output_dir => Dir},
            rebar3_formatter:format(RelPath, Opts),
            els_text_edit:diff_files(Path, OutFile)
          end,
    TextEdits = tempdir:mktmp(Fun),
    lager:info("format_document: [TextEdits=~p]", [TextEdits]),
    {ok, TextEdits}.


-spec rangeformat_document(uri(), els_document:document(), range()
                          , formatting_options())
                          -> {ok, [text_edit()]}.
rangeformat_document(Uri, _Document, Range, Options) ->
    lager:info("rangeformat_document: ~p", [{Uri, Range, Options}]),
    {ok, []}.

-spec ontypeformat_document(binary(), els_document:document(), number()
                           , number(), string(), formatting_options())
                           -> {ok, [text_edit()]}.
ontypeformat_document(Uri, _Document, Line, Col, Char, Options) ->
    lager:info("ontypeformat_document: ~p", [{Uri, Line, Col, Char, Options}]),
    {ok, []}.
