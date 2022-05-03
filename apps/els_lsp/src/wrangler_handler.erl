%%==============================================================================
%% A module to call Wrangler functions.
%% If wrangler is not configured, neutral elements will be returned.
%%==============================================================================

-module(wrangler_handler).
-export([ is_enabled/0
        , get_code_actions/2
        , get_code_lenses/1
        , enabled_commands/0
        , execute_command/2
        , get_highlights/3
        , semantic_token_types/0
        , semantic_token_modifiers/0
        , get_semantic_tokens/1]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").


-spec is_enabled() -> boolean().
is_enabled() ->
  case els_config:get(wrangler) of
    notconfigured -> false;
    _ -> true
  end.

-spec get_code_actions(uri(), range()) -> [map()].
get_code_actions(Uri, Range) ->
  case is_enabled() of
    true -> wls_code_actions:get_actions(Uri, Range);
    false -> []
  end.

-spec get_code_lenses(els_dt_document:item()) -> [els_code_lens:lens()].
get_code_lenses(Document) ->
  case is_enabled() of
    true ->
      Lenses = [wls_code_lens:lenses(Id, Document) || Id <- wls_code_lens:enabled_lenses()],
      ?LOG_INFO("Wrangler Code Lenses: ~p", [Lenses]),
      Lenses;
    false -> []
  end.

-spec get_highlights(uri(), integer(), integer()) -> 'null' | [map()].
get_highlights(Uri, Line, Character) ->
  case is_enabled() of
    true ->
      Highlights = wls_highlight:get_highlights(Uri, {Line, Character}),
      ?LOG_INFO("Wrangler Highlights: ~p", [Highlights]),
      Highlights;
    false -> null
  end.


-spec execute_command(els_command:command_id(), [any()]) -> boolean().
execute_command(Command, Arguments) ->
  case is_enabled() of
    true ->
      case lists:member(Command, wls_execute_command_provider:enabled_commands()) of
        true ->
          wls_execute_command_provider:execute_command(Command, Arguments),
          true;
        false -> false
      end;
    false -> false
  end.


-spec enabled_commands() -> [els_command:command_id()].
enabled_commands() ->
  case is_enabled() of
    true ->
      Commands = wls_execute_command_provider:enabled_commands(),
      ?LOG_INFO("Wrangler Enabled Commands: ~p", [Commands]),
      Commands;
    false -> []
  end.

-spec semantic_token_types() -> any().
semantic_token_types() ->
  case is_enabled() of
    true -> wls_semantic_tokens:token_types();
    false -> []
  end.

-spec semantic_token_modifiers() -> any().
semantic_token_modifiers() ->
  case is_enabled() of
    true -> wls_semantic_tokens:token_modifiers();
    false -> []
  end.

-spec get_semantic_tokens(uri()) -> [integer()].
get_semantic_tokens(Uri) ->
  case is_enabled() of
    true ->
      SemanticTokens = wls_semantic_tokens:semantic_tokens(Uri),
      ?LOG_INFO("Wrangler Semantic Tokens: ~p", [SemanticTokens]),
      SemanticTokens;
    false -> []
  end.
