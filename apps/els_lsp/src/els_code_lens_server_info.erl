%%==============================================================================
%% Code Lens: server_info
%%==============================================================================

-module(els_code_lens_server_info).

-behaviour(els_code_lens).
-export([ command/3
        , is_default/0
        , pois/1
        , precondition/1
        ]).

-include("els_lsp.hrl").

-spec command(els_dt_document:item(), poi(), els_code_lens:state()) ->
        els_command:command().
command(_Document, _POI, _State) ->
  Title = title(),
  CommandId = <<"server-info">>,
  CommandArgs = [],
  els_command:make_command(Title, CommandId, CommandArgs).

-spec is_default() -> boolean().
is_default() ->
  false.

-spec precondition(els_dt_document:item()) -> boolean().
precondition(_Document) ->
  true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(_Document) ->
  %% Return a dummy POI on the first line
  [els_poi:new(#{from => {1, 1}, to => {2, 1}}, dummy, dummy)].

-spec title() -> binary().
title() ->
  Root = filename:basename(els_uri:path(els_config:get(root_uri))),
  <<"Erlang LS (in ", Root/binary, ") info">>.
