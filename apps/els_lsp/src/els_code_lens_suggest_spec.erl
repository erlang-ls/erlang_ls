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
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: els_typer:info() | {error, atom()}.

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
            {Reason, Explanation} =
                case explanation(E) of
                    undefined -> {unknown, "Cannot extract typer info."};
                    Expl -> Expl
                end,
            Fmt =
                "~s: ~s~n"
                "Class: ~p~n"
                "Exception: ~p~n"
                "Stacktrace: ~p~n",
            ?LOG_WARNING(Fmt, [Reason, Explanation, C, E, S]),
            {error, Reason}
    end.

-spec command(els_dt_document:item(), els_poi:poi(), state()) -> els_command:command().
command(_Document, _POI, {error, Reason}) ->
    CommandId = <<"suggest-spec">>,
    Message = io_lib:format("Cannot extract specs ('~p', check logs for details)", [Reason]),
    Title = els_utils:to_binary(Message),
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

-spec pois(els_dt_document:item()) -> [els_poi:poi()].
pois(Document) ->
    Functions = els_dt_document:pois(Document, [function]),
    Specs = els_dt_document:pois(Document, [spec]),
    SpecsIds = [Id || #{id := Id} <- Specs],
    [POI || #{id := Id} = POI <- Functions, not lists:member(Id, SpecsIds)].

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec get_type_spec(els_poi:poi(), els_typer:info()) -> binary().
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

%% @doc Explain the given exception, try to suggest a solution.
-spec explanation(any()) -> {atom(), string()} | undefined.
explanation({dialyzer_error, "Could not read PLT file " ++ _ = Reason}) ->
    Pattern = "Could not read PLT file (.*) (not_valid|no_such_file)",
    case re:run(Reason, Pattern, [{capture, all_but_first, list}]) of
        {match, [File, "no_such_file"]} ->
            {no_plt, "Could not read the PLT file, check that 'plt_path' is correct: " ++ File};
        {match, [File, "not_valid"]} ->
            {invalid_plt, "Could not read the PLT file because it is not valid: " ++ File};
        _ ->
            undefined
    end;
explanation({dialyzer_error, "Old PLT file " ++ Message}) ->
    {old_plt, "Dialyzer could not read the PLT file because it is incompatible: " ++ Message};
explanation(_) ->
    undefined.
