-module(els_signature_help_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    is_enabled/0,
    handle_request/1,
    trigger_characters/0
]).

-type item() :: {remote, atom(), atom()} | {local, atom()}.
-type parameter_number() :: non_neg_integer().
%% Parameter numbers are 0-indexed.

-spec trigger_characters() -> [binary()].
trigger_characters() ->
    [<<"(">>, <<",">>, <<")">>].

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() ->
    false.

-spec handle_request(els_provider:provider_request()) ->
    {response, signature_help() | null}.
handle_request({signature_help, Params}) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    {ok, #{text := Text} = Document} = els_utils:lookup_document(Uri),
    Prefix = els_text:line(Text, Line, Character),
    Tokens = lists:reverse(els_text:tokens(Prefix)),
    case find_signature(Tokens, Text, Line - 1) of
        {ok, Item, ActiveParameter} ->
            {response, signatures(Document, Item, ActiveParameter)};
        none ->
            {response, null}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec find_signature(
    Tokens :: [tuple()],
    Text :: binary(),
    Line :: non_neg_integer()
) ->
    {ok, item(), parameter_number()} | none.
find_signature(Tokens, Text, Line) ->
    find_signature(Tokens, [0], Text, Line).

-spec find_signature(
    Tokens :: [tuple()],
    ParameterStack :: [parameter_number()],
    Text :: binary(),
    Line :: non_neg_integer()
) ->
    {ok, item(), parameter_number()} | none.
%% An unmatched open parenthesis is the start of a signature.
find_signature([{'(', _} | Rest], [ActiveParameter], _Text, _Line) ->
    case Rest of
        %% Check for "[...] module:func("
        [{atom, _, Func}, {':', _}, {atom, _, Module} | _Rest] ->
            {ok, {remote, Module, Func}, ActiveParameter};
        %% Check for "-attribute("
        [{atom, _, _Attribute}, {'-', _} | _Rest] ->
            none;
        %% Check for "[...] func("
        [{atom, _, Func} | _Rest] ->
            {ok, {local, Func}, ActiveParameter};
        _Tokens ->
            none
    end;
%% A comma outside of any data structure (list, tuple, map, or binary) is a
%% separator between arguments, so we increment the active parameter count.
find_signature([{',', _} | Rest], [ActiveParameter | ParameterStack], Text, Line) ->
    find_signature(Rest, [ActiveParameter + 1 | ParameterStack], Text, Line);
%% Calls may contain any sort of expression but not statements, so when we
%% see a '.', we know we've failed to find a signature.
find_signature([{dot, _} | _Rest], _ParameterStack, _Text, _Line) ->
    none;
%% When closing a scope, push a new parameter counter onto the stack.
find_signature([{ScopeClose, _} | Rest], ParameterStack, Text, Line) when
    ScopeClose =:= ')';
    ScopeClose =:= '}';
    ScopeClose =:= ']';
    ScopeClose =:= '>>'
->
    find_signature(Rest, [0 | ParameterStack], Text, Line);
%% When opening a scope, pop the extra parameter counter if it exists.
find_signature([{ScopeOpen, _} | Rest], ParameterStack, Text, Line) when
    ScopeOpen =:= '(';
    ScopeOpen =:= '{';
    ScopeOpen =:= '[';
    ScopeOpen =:= '<<'
->
    ParameterStack1 =
        case ParameterStack of
            [_] -> [0];
            [_Head | Tail] -> Tail
        end,
    find_signature(Rest, ParameterStack1, Text, Line);
%% Discard any other tokens
find_signature([_ | Rest], ParameterStack, Text, Line) ->
    find_signature(Rest, ParameterStack, Text, Line);
%% If there are no lines remaining in the file, then we failed to find any
%% signatures and are done.
find_signature([], _ParameterStack, _Text, 0) ->
    none;
%% If we have exhausted the set of tokens on this line, scan backwards a line
%% (up in the document) since expressions may be split across multiple lines.
find_signature([], ParameterStack, Text, Line) ->
    LineContents = els_text:line(Text, Line),
    Tokens = lists:reverse(els_text:tokens(LineContents)),
    find_signature(Tokens, ParameterStack, Text, Line - 1).

-spec signatures(els_dt_document:item(), item(), parameter_number()) ->
    signature_help() | null.
signatures(Document, Item, ActiveParameter) ->
    {Module, Function, POIs} =
        case Item of
            {local, F} ->
                #{uri := Uri} = Document,
                M = els_uri:module(Uri),
                LocalPOIs = els_scope:local_and_included_pois(Document, function),
                {M, F, LocalPOIs};
            {remote, M, F} ->
                {M, F, exported_function_pois(M)}
        end,
    SignaturePOIs =
        lists:sort(
            fun(#{id := {_, AArity}}, #{id := {_, BArity}}) -> AArity < BArity end,
            [
                POI
             || #{id := {POIFunc, _Arity}} = POI <- POIs,
                POIFunc =:= Function
            ]
        ),
    ?LOG_DEBUG(
        "Signature Help. [item=~p] [pois=~p]",
        [Item, SignaturePOIs]
    ),
    case SignaturePOIs of
        [] ->
            null;
        [_ | _] ->
            %% The active signature is the signature with the smallest arity
            %% that is at least as large as the active parameter, defaulting
            %% to the highest arity signature. The active signature is zero
            %% indexed.
            ActiveSignature =
                index_where(
                    fun(#{id := {_, Arity}}) -> Arity > ActiveParameter end,
                    SignaturePOIs,
                    length(SignaturePOIs) - 1
                ),
            #{
                activeParameter => ActiveParameter,
                activeSignature => ActiveSignature,
                signatures => [signature_item(Module, POI) || POI <- SignaturePOIs]
            }
    end.

-spec signature_item(atom(), els_poi:poi()) -> signature_information().
signature_item(Module, #{data := #{args := Args}, id := {Function, Arity}}) ->
    DocEntries = els_docs:function_docs('remote', Module, Function, Arity),
    #{
        documentation => els_markup_content:new(DocEntries),
        label => label(Function, Args),
        parameters => [#{label => els_utils:to_binary(Name)} || {_Index, Name} <- Args]
    }.

-spec exported_function_pois(atom()) -> [els_poi:poi()].
exported_function_pois(Module) ->
    case els_utils:find_module(Module) of
        {ok, Uri} ->
            case els_utils:lookup_document(Uri) of
                {ok, Document} ->
                    Exports = [
                        FA
                     || #{id := FA} <- els_scope:local_and_included_pois(Document, export_entry)
                    ],
                    [
                        POI
                     || #{id := FA} = POI <- els_scope:local_and_included_pois(Document, function),
                        lists:member(FA, Exports)
                    ];
                {error, _} ->
                    []
            end;
        {error, _} ->
            []
    end.

-spec label(atom(), [tuple()]) -> binary().
label(Function, Args0) ->
    ArgList = ["(", string:join([Name || {_Index, Name} <- Args0], ", "), ")"],
    els_utils:to_binary([atom_to_binary(Function, utf8) | ArgList]).

-spec index_where(Predicate, list(), Default) -> non_neg_integer() | Default when
    Predicate :: fun((term()) -> boolean()),
    Default :: term().
index_where(Predicate, List, Default) ->
    {IndexedList, _Acc} = lists:mapfoldl(fun(Item, Acc) -> {{Acc, Item}, Acc + 1} end, 0, List),
    case lists:search(fun({_Index, Item}) -> Predicate(Item) end, IndexedList) of
        {value, {Index, _Item}} -> Index;
        false -> Default
    end.
