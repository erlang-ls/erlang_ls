-module(els_erlang_provider).

-behaviour(els_provider).

-export([
    is_enabled/0,
    handle_request/1
]).

-include("els_lsp.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any()) -> {response, any()}.
handle_request({edoc, Params}) ->
    #{
      <<"textDocument">> := #{<<"uri">> := Uri}
     } = Params,
    %% M = els_uri:module(Uri),
    %% {ok, Doc} = els_utils:lookup_document(Uri),
    %% POIs = els_dt_document:pois(Doc, [function]),
    %% TODO: Background job
    %% TODO: Markdown?
    %% Output = [begin
    %%               #{value := Value} = els_markup_content:new(els_docs:edoc(M, F, A)),
    %%               Value
    %%           end || #{id := {F, A}} <- POIs],
    %% {response, #{name => unicode:characters_to_binary(lists:join("\n\n", Output))}}.
    {response, #{name => get_docs(Uri)}}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec get_docs(uri()) -> binary().
get_docs(Uri) ->
    Module = els_uri:module(Uri),
    case els_docs:get_edoc_chunk(Module, Uri) of
        error ->
            <<"Error while producing EDoc">>;
        {ok, Chunk} ->
            ModuleDocs = shell_docs:render(Module, Chunk),
            %% TODO: Is there a single API?
            {ok, [Document]} = els_dt_document:lookup(Uri),
            Functions = els_dt_document:pois(Document, [function]),
            FunctionsDocs = [shell_docs:render(Module, F, A, Chunk) || #{id := {F, A}} <- Functions],
            unicode:characters_to_binary([ModuleDocs | FunctionsDocs])
    end.
