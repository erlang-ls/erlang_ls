%%==============================================================================
%% Hover Provider
%%==============================================================================
-module(els_hover_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: any().
-type doc_entry() :: { 'text' | 'h1' | 'h2' | 'h3' | 'h4'
                     | 'code_block_line' | 'code_block_begin' | 'code_block_end'
                     | 'code_line' | 'code_inline'
                     , string()
                     }.
-export_type([ doc_entry/0 ]).

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({hover, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Doc} = els_utils:lookup_document(Uri),
  case els_dt_document:get_element_at_pos(Doc, Line + 1, Character + 1) of
    [] ->
      {null, State};
    [POI|_] ->
      Entries = els_docs:docs(els_uri:module(Uri), POI),
      {#{contents => contents(Entries)}, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec contents([doc_entry()]) -> markup_content().
contents(Entries) ->
  MarkupKind = markup_kind(),
  Value = value(MarkupKind, Entries),
  #{ kind => MarkupKind
   , value => Value
   }.

-spec value(markup_kind(), [doc_entry()]) -> binary().
value(MarkupKind, Entries) ->
  Separator = separator(MarkupKind),
  FormattedEntries = [format_entry(Entry, MarkupKind) || Entry <- Entries],
  unicode:characters_to_binary(string:join(FormattedEntries, Separator)).

-spec markup_kind() -> markup_kind().
markup_kind() ->
  ContentFormat =
    case els_config:get(capabilities) of
      #{<<"textDocument">> := #{<<"hover">> := #{<<"contentFormat">> := X}}} ->
        X;
      _ ->
        []
    end,
  case lists:member(atom_to_binary(?MARKDOWN, utf8), ContentFormat) of
    true  -> ?MARKDOWN;
    false -> ?PLAINTEXT
  end.

-spec separator(markup_kind()) -> string().
separator(?MARKDOWN) ->
  "\n\n";
separator(?PLAINTEXT) ->
  "\n".

-spec format_entry(doc_entry(), markup_kind()) -> string().
format_entry({h1, String}, _MarkupKind) ->
  "# " ++ String;
format_entry({h2, String}, _MarkupKind) ->
  "## " ++ String;
format_entry({h3, String}, _MarkupKind) ->
  "### " ++ String;
format_entry({h4, String}, _MarkupKind) ->
  "#### " ++ String;
format_entry({code_block_line, String}, _MarkupKind) ->
  "  " ++ String;
format_entry({code_block_begin, _Language}, ?PLAINTEXT) ->
  "";
format_entry({code_block_begin, Language}, ?MARKDOWN) ->
  "```" ++ Language;
format_entry({code_block_end, _Language}, ?PLAINTEXT) ->
  "";
format_entry({code_block_end, _Language}, ?MARKDOWN) ->
  "```";
format_entry({code_inline, String}, ?PLAINTEXT) ->
  String;
format_entry({code_line, String}, ?MARKDOWN) ->
  "```erlang\n" ++ String ++ "\n```";
format_entry({_Kind, String}, _MarkupKind) ->
  String.
