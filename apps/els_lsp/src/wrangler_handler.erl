%%==============================================================================
%% A module to call Wrangler functions.
%% If wrangler is not configured, neutral elements will be returned.
%%==============================================================================

-module(wrangler_handler).
-export([
        is_enabled/0,
        wrangler_config/0,
        get_code_actions/2,
        get_code_lenses/1,
        enabled_commands/0,
        execute_command/2,
        get_highlights/3,
        semantic_token_types/0,
        semantic_token_modifiers/0,
        get_semantic_tokens/1]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Configuration related functions.
%%==============================================================================

%% Check if Wrangler is enabled in the config file.
-spec is_enabled() -> boolean().
is_enabled() ->
    case els_config:get(wrangler) of
        notconfigured -> false;
        Config ->
            maps:get("enabled", Config, false)
    end.

%% Returns Wrangler`s config from the config file.
%% Used by Wrangler.
-spec wrangler_config() -> map().
wrangler_config() ->
    els_config:get(wrangler).

%% Returns the semantic token types defined by Wrangler.
%% Used to register server capabilities.
-spec semantic_token_types() -> any().
semantic_token_types() ->
    case is_enabled() of
        true -> wls_semantic_tokens:token_types();
        false -> []
    end.

%% Returns the semantic token modifiers defined by Wrangler.
%% Used to register server capabilities.
-spec semantic_token_modifiers() -> any().
semantic_token_modifiers() ->
    case is_enabled() of
        true -> wls_semantic_tokens:token_modifiers();
        false -> []
    end.

%% Returns the enabled Wrangler commands.
%% Used to register server capabilities.
-spec enabled_commands() -> [els_command:command_id()].
enabled_commands() ->
    case is_enabled() of
        true ->
            Commands = wls_execute_command_provider:enabled_commands(),
            ?LOG_INFO("Wrangler Enabled Commands: ~p", [Commands]),
            Commands;
        false -> []
    end.

%%==============================================================================
%% Getters for the language features provided by Wrangler.
%%==============================================================================

-spec get_code_actions(uri(), range()) -> [map()].
get_code_actions(Uri, Range) ->
    case is_enabled() of
        true ->
            case wls_code_actions:get_actions(Uri, Range) of
                [] -> [];
                Actions ->
                    ?LOG_INFO("Wrangler Code Actions: ~p", [Actions]),
                    Actions
            end;
        false -> []
    end.

-spec get_code_lenses(els_dt_document:item()) -> [els_code_lens:lens()].
get_code_lenses(Document) ->
    case is_enabled() of
        true ->
            case lists:flatten([wls_code_lens:lenses(Id, Document)
                                    || Id <- wls_code_lens:enabled_lenses()])
            of
                [] -> [];
                Lenses ->
                    ?LOG_INFO("Wrangler Code Lenses: ~p", [Lenses]),
                    Lenses
            end;
        false -> []
    end.

-spec get_highlights(uri(), integer(), integer()) -> 'null' | [map()].
get_highlights(Uri, Line, Character) ->
    case is_enabled() of
        true ->
            case wls_highlight:get_highlights(Uri, {Line, Character}) of
                null -> null;
                Highlights ->
                    ?LOG_INFO("Wrangler Highlights: ~p", [Highlights]),
                    Highlights
            end;
        false -> null
    end.

-spec get_semantic_tokens(uri()) -> [integer()].
get_semantic_tokens(Uri) ->
    case is_enabled() of
        true ->
            case wls_semantic_tokens:semantic_tokens(Uri) of
                [] -> [];
                SemanticTokens ->
                    ?LOG_INFO("Wrangler Semantic Tokens: ~p", [SemanticTokens]),
                    SemanticTokens
            end;
        false -> []
    end.

%%==============================================================================
%% Passing commands to Wrangler.
%%==============================================================================

-spec execute_command(els_command:command_id(), [any()]) -> boolean().
execute_command(Command, Arguments) ->
    case is_enabled() of
        true ->
            case lists:member(Command,
                wls_execute_command_provider:enabled_commands())
            of
                true ->
                    wls_execute_command_provider:execute_command(Command, Arguments),
                    true;
                false -> false
            end;
        false -> false
    end.
