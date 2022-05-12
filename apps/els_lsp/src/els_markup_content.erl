-module(els_markup_content).

-export([new/1]).

-include_lib("els_core/include/els_core.hrl").

-type doc_entry() :: {
    'text'
    | 'h1'
    | 'h2'
    | 'h3'
    | 'h4'
    | 'code_block_line'
    | 'code_block_begin'
    | 'code_block_end'
    | 'code_line'
    | 'code_inline',
    string()
}.
-export_type([doc_entry/0]).

-spec new([doc_entry()]) -> markup_content().
new(Entries) ->
    MarkupKind = markup_kind(),
    Value = value(MarkupKind, Entries),
    #{
        kind => MarkupKind,
        value => Value
    }.

%%------------------------------------------------------------------------------
%% @doc markup_kind/0
%% Return the Markup Kind preferred by the client, according to the specified
%% capabilities
%% @end
%%------------------------------------------------------------------------------
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
        true -> ?MARKDOWN;
        false -> ?PLAINTEXT
    end.

-spec value(markup_kind(), [doc_entry()]) -> binary().
value(MarkupKind, Entries) ->
    Separator = separator(MarkupKind),
    FormattedEntries = [format_entry(Entry, MarkupKind) || Entry <- Entries],
    unicode:characters_to_binary(string:join(FormattedEntries, Separator)).

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
