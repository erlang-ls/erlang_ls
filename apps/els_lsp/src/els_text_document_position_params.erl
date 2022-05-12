-module(els_text_document_position_params).

-export([
    uri/1,
    line/1,
    character/1,
    uri_line_character/1
]).

-include("els_lsp.hrl").

%% Since map keys are binary (for now), we can't really be more specific.
-type params() :: map().

-spec uri(params()) -> uri().
uri(Params) ->
    maps:get(<<"uri">>, maps:get(<<"textDocument">>, Params)).

-spec line(params()) -> non_neg_integer().
line(Params) ->
    maps:get(<<"line">>, maps:get(<<"position">>, Params)).

-spec character(params()) -> non_neg_integer().
character(Params) ->
    maps:get(<<"line">>, maps:get(<<"position">>, Params)).

-spec uri_line_character(params()) ->
    {uri(), non_neg_integer(), non_neg_integer()}.
uri_line_character(Params) ->
    {uri(Params), line(Params), character(Params)}.
