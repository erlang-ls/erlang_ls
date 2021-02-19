%%==============================================================================
%% Code Lens: export : show export status of functions, allow toggling it.
%%==============================================================================

-module(els_code_lens_show_exported).

-behaviour(els_code_lens).
-export([ command/1
        , command_args/2
        , is_default/0
        , pois/1
        , precondition/1
        , title/2
        ]).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-spec command(poi()) -> els_command:command_id().
command(_POI) ->
  <<"show-exported">>.

-spec command_args(els_dt_document:item(), poi()) -> [any()].
command_args(_Document, _POI) ->
  [].

-spec is_default() -> boolean().
is_default() ->
  false.

-spec precondition(els_dt_document:item()) -> boolean().
precondition(Document) ->
  case Document of
    #{kind := module} ->
      %% A behaviour is defined by the presence of one more functions
       (els_dt_document:pois(Document, [function])) > 0;
    _ ->
      false
  end.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  els_dt_document:pois(Document, [function]).

-spec title(els_dt_document:item(), poi()) -> binary().
title(#{uri := URI} = Document, POI) ->
  Exports = els_completion_provider:resolve_exports(Document, [POI]
                                                   , function, true, true),
  NumReferences = length(els_references_provider:find_references(URI, POI)),
  ExpMsg = case Exports of
    [] -> io_lib:format("local", []);
    _  -> io_lib:format("exported", [])
  end,
  RefMsg = io_lib:format("(~b references)", [NumReferences]),
  els_utils:to_binary([ExpMsg, " ", RefMsg]).
