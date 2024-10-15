-module(els_code_action_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

-include("els_lsp.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {response, any()}.
handle_request({document_codeaction, Params}) ->
    %% TODO: Make code actions run async?
    %% TODO: Extract document here
    #{
        <<"textDocument">> := #{<<"uri">> := Uri},
        <<"range">> := RangeLSP,
        <<"context">> := Context
    } = Params,
    Result = code_actions(Uri, RangeLSP, Context),
    {response, Result}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc Result: `(Command | CodeAction)[] | null'
-spec code_actions(uri(), range(), code_action_context()) -> [map()].
code_actions(Uri, Range, #{<<"diagnostics">> := Diagnostics}) ->
    lists:usort(
        lists:flatten([make_code_actions(Uri, D) || D <- Diagnostics]) ++
            wrangler_handler:get_code_actions(Uri, Range) ++
            els_code_actions:extract_function(Uri, Range) ++
            els_code_actions:bump_variables(Uri, Range)
    ).

-spec make_code_actions(uri(), map()) -> [map()].
make_code_actions(
    Uri,
    #{<<"message">> := Message, <<"range">> := Range} = Diagnostic
) ->
    Data = maps:get(<<"data">>, Diagnostic, <<>>),
    make_code_actions(
        [
            {"function (.*) is unused", fun els_code_actions:export_function/4},
            {"variable '(.*)' is unused", fun els_code_actions:ignore_variable/4},
            {"variable '(.*)' is unbound", fun els_code_actions:suggest_variable/4},
            {"undefined macro '(.*)'", fun els_code_actions:add_include_lib_macro/4},
            {"undefined macro '(.*)'", fun els_code_actions:define_macro/4},
            {"undefined macro '(.*)'", fun els_code_actions:suggest_macro/4},
            {"record (.*) undefined", fun els_code_actions:add_include_lib_record/4},
            {"record (.*) undefined", fun els_code_actions:define_record/4},
            {"record (.*) undefined", fun els_code_actions:suggest_record/4},
            {"field (.*) undefined in record (.*)", fun els_code_actions:suggest_record_field/4},
            {"Module name '(.*)' does not match file name '(.*)'",
                fun els_code_actions:fix_module_name/4},
            {"Unused macro: (.*)", fun els_code_actions:remove_macro/4},
            {"function (.*) undefined", fun els_code_actions:create_function/4},
            {"function (.*) undefined", fun els_code_actions:suggest_function/4},
            {"Cannot find definition for function (.*)", fun els_code_actions:suggest_function/4},
            {"Cannot find module (.*)", fun els_code_actions:suggest_module/4},
            {"Function (.+):(.+) is not exported.",
                fun els_code_actions:export_external_function/4},
            {"Unused file: (.*)", fun els_code_actions:remove_unused/4},
            {"Atom typo\\? Did you mean: (.*)", fun els_code_actions:fix_atom_typo/4},
            {"undefined callback function (.*) \\\(behaviour '(.*)'\\\)",
                fun els_code_actions:undefined_callback/4}
        ],
        Uri,
        Range,
        Data,
        Message
    ).

-spec make_code_actions([{string(), Fun}], uri(), range(), binary(), binary()) ->
    [map()]
when
    Fun :: fun((uri(), range(), binary(), [binary()]) -> [map()]).
make_code_actions([], _Uri, _Range, _Data, _Message) ->
    [];
make_code_actions([{RE, Fun} | Rest], Uri, Range, Data, Message) ->
    Actions =
        case re:run(Message, RE, [{capture, all_but_first, binary}]) of
            {match, Matches} ->
                Fun(Uri, Range, Data, Matches);
            nomatch ->
                []
        end,
    Actions ++ make_code_actions(Rest, Uri, Range, Data, Message).
