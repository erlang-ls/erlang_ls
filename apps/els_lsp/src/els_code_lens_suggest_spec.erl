%%==============================================================================
%% Code Lens: suggest_spec
%%==============================================================================
-module(els_code_lens_suggest_spec).

-behaviour(els_code_lens).
-export([
    init/1,
    command/3,
    is_default/0,
    pois/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: els_typer:info() | 'no_info'.

%%==============================================================================
%% Callback functions for the els_code_lens behaviour
%%==============================================================================
-spec init(els_dt_document:item()) -> state().
init(#{uri := Uri} = _Document) ->
    try els_typer:get_info(Uri) of
        Info ->
            Info
    catch
        C:E:S ->
            Fmt =
                "Cannot extract typer info.~n"
                "Class: ~p~n"
                "Exception: ~p~n"
                "Stacktrace: ~p~n",
            ?LOG_WARNING(Fmt, [C, E, S]),
            'no_info'
    end.

-spec command(els_dt_document:item(), poi(), state()) -> els_command:command().
command(_Document, _POI, 'no_info') ->
    CommandId = <<"suggest-spec">>,
    Title = <<"Cannot extract specs (check logs for details)">>,
    els_command:make_command(Title, CommandId, []);
command(Document, #{range := #{from := {Line, _}}} = POI, Info) ->
    #{uri := Uri} = Document,
    CommandId = <<"suggest-spec">>,
    Spec = get_type_spec(POI, Info),
    Title = truncate_spec_title(Spec, spec_title_max_length()),
    CommandArgs = [
        #{
            uri => Uri,
            line => Line,
            spec => Spec
        }
    ],
    els_command:make_command(Title, CommandId, CommandArgs).

-spec is_default() -> boolean().
is_default() ->
    true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
    Functions = els_dt_document:pois(Document, [function]),
    Specs = els_dt_document:pois(Document, [spec]),
    SpecsIds = [Id || #{id := Id} <- Specs],
    [POI || #{id := Id} = POI <- Functions, not lists:member(Id, SpecsIds)].

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec get_type_spec(poi(), els_typer:info()) -> binary().
get_type_spec(POI, Info) ->
    #{id := {Function, Arity}} = POI,
    Spec = els_typer:get_type_spec(Function, Arity, Info),
    re:replace(Spec, ",", ", ", [global, {return, binary}]).

-spec truncate_spec_title(binary(), integer()) -> binary().
truncate_spec_title(Spec, MaxLength) ->
    Length = string:length(Spec),
    case Length > MaxLength of
        true ->
            Title = unicode:characters_to_binary(
                string:slice(Spec, 0, MaxLength - 3)
            ),
            <<Title/binary, "...">>;
        false ->
            Spec
    end.

-spec spec_title_max_length() -> integer().
spec_title_max_length() ->
    application:get_env(els_core, suggest_spec_title_max_length, 100).
