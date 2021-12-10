%%==============================================================================
%% Sheldon diagnostics
%%==============================================================================

-module(els_sheldon_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================

-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ init/0
        , is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec init() -> ok.
init() ->
  %% By default "sheldon" is not started by reason that he spend few seconds
  %% to prepare and load dictionary. The "sheldon" shoulld be load only once
  %% when diagnostic is enabled.
  application:ensure_all_started(sheldon),
  ok.

-spec is_default() -> boolean().
is_default() ->
  false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case els_utils:project_relative(Uri) of
    {error, not_relative} -> [];
    RelFile ->
      try
        RegEx = "[_@./#&+-=*]",
        rebar3_sheldon_ast:spellcheck([RelFile], RegEx)
      of
        [] -> [];
        Problems -> format_diagnostics(Problems)
      catch Type:Error:Stacktrace ->
        ?LOG_WARNING( "Sheldon error: [type=~p] [error=~p] [stacktrace=~p]"
                    , [Type, Error, Stacktrace]
                    ),
        []
      end
    end.

-spec source() -> binary().
source() ->
  <<"Sheldon">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec format_diagnostics(any()) -> [map()].
format_diagnostics(Data) ->
  R = format_rules(Data),
  lists:flatten(R).

-spec format_rules([any()]) -> [[map()]].
format_rules([]) ->
  [];
format_rules([#{reason := #{misspelled_words := Miss}} = Data | EItems]) ->
  ItemDiags = format_item(Miss, Data),
  [ItemDiags | format_rules(EItems)].

-spec format_item([map()], map()) -> [map()].
format_item([#{candidates := [], word := Word} | Items], Data) ->
  #{line := Line, type := Type} = Data,
  Msg = format_text("The word ~p in ~p is unknown.", [Word, Type]),
  Diagnostic = diagnostic(Msg, Line, ?DIAGNOSTIC_WARNING),
  [Diagnostic | format_item(Items, Data)];
format_item([#{candidates := Candidates, word := Word} | Items], Data) ->
  #{line := Line, type := Type} = Data,
  FormatCandidates = format_sheldon_candidates(Candidates, []),
  Text = "The word ~p in ~p is unknown. Maybe you wanted to use ~ts?",
  Msg = format_text(Text, [Word, Type, FormatCandidates]),
  Diagnostic = diagnostic(Msg, Line, ?DIAGNOSTIC_WARNING),
  [Diagnostic | format_item(Items, Data)];
format_item([], _) ->
  [].

-spec diagnostic( any(), integer(),  els_diagnostics:severity()) -> map().
diagnostic(Msg, Ln, Severity) ->
  Range   = els_protocol:range(#{from => {Ln, 1}, to => {Ln + 1, 1}}),
  Message = els_utils:to_binary(Msg),
  #{ range    => Range
    , severity => Severity
    , code     => spellcheck
    , source   => source()
    , message  => Message
    , relatedInformation => []
    }.

-spec format_sheldon_candidates([any()], [[[any()] | char()]]) -> list().
format_sheldon_candidates([], Acc) ->
  Acc;
format_sheldon_candidates([Candidate], Acc) ->
  [Acc, format_text("~p", [Candidate])];
format_sheldon_candidates([Candidate | T], Acc) ->
  format_sheldon_candidates(T, [Acc, format_text("~p or ", [Candidate])]).

-spec format_text(string(), list()) -> string().
format_text(Text, Args) ->
  Formatted = io_lib:format(Text, Args),
  unicode:characters_to_list(Formatted).
