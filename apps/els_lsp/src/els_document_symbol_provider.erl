-module(els_document_symbol_provider).

-behaviour(els_provider).

-export([
    is_enabled/0,
    handle_request/2
]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {response, any()}.
handle_request({document_symbol, Params}, _State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    Symbols = symbols(Uri),
    case Symbols of
        [] -> {response, null};
        _ -> {response, Symbols}
    end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec symbols(uri()) -> [map()].
symbols(Uri) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:pois(Document, [
        function,
        define,
        record,
        type_definition
    ]),
    lists:reverse([poi_to_symbol(Uri, POI) || POI <- POIs]).

-spec poi_to_symbol(uri(), els_poi:poi()) -> symbol_information().
poi_to_symbol(Uri, POI) ->
    #{range := Range, kind := Kind, id := Id} = POI,
    #{
        name => symbol_name(Kind, Id),
        kind => symbol_kind(Kind),
        location => #{
            uri => Uri,
            range => els_protocol:range(Range)
        }
    }.

-spec symbol_kind(els_poi:poi_kind()) -> symbol_kind().
symbol_kind(function) -> ?SYMBOLKIND_FUNCTION;
symbol_kind(define) -> ?SYMBOLKIND_CONSTANT;
symbol_kind(record) -> ?SYMBOLKIND_STRUCT;
symbol_kind(type_definition) -> ?SYMBOLKIND_TYPE_PARAMETER.

-spec symbol_name(els_poi:poi_kind(), any()) -> binary().
symbol_name(function, {F, A}) ->
    els_utils:to_binary(io_lib:format("~s/~p", [F, A]));
symbol_name(define, {Name, Arity}) ->
    els_utils:to_binary(io_lib:format("~s/~p", [Name, Arity]));
symbol_name(define, Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
symbol_name(record, Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
symbol_name(type_definition, {Name, Arity}) ->
    els_utils:to_binary(io_lib:format("~s/~p", [Name, Arity])).
