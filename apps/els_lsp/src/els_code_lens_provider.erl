-module(els_code_lens_provider).

-behaviour(els_provider).
-export([
    is_enabled/0,
    options/0,
    handle_request/2
]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
    #{resolveProvider => false}.

-spec handle_request(any(), any()) -> {async, uri(), pid()}.
handle_request({document_codelens, Params}, _State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    ?LOG_DEBUG("Starting lenses job [uri=~p]", [Uri]),
    Job = run_lenses_job(Uri),
    {async, Uri, Job}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec run_lenses_job(uri()) -> pid().
run_lenses_job(Uri) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    Config = #{
        task =>
            fun(Doc, _) ->
                lists:flatten(
                    [
                        els_code_lens:lenses(Id, Doc)
                     || Id <- els_code_lens:enabled_lenses()
                    ]
                    ++ wrangler_handler:get_code_lenses(Doc)
                )
            end,
        entries => [Document],
        title => <<"Lenses">>,
        on_complete =>
            fun(Lenses) ->
                els_provider ! {result, Lenses, self()},
                ok
            end
    },
    {ok, Pid} = els_background_job:new(Config),
    Pid.
