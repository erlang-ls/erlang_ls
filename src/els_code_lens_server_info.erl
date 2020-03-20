%%==============================================================================
%% Code Lens: server_info
%%==============================================================================

-module(els_code_lens_server_info).

-export([ command/0
        , is_default/0
        , lenses/1
        ]).

-behaviour(els_code_lens).

-spec command() -> els_command:command_id().
command() ->
  <<"server-info">>.

-spec is_default() -> boolean().
is_default() ->
  false.

%% @doc Given a Document, returns the available lenses
-spec lenses(els_dt_document:item()) -> [els_code_lens:lens()].
lenses(_Document) ->
  Root = filename:basename(els_uri:path(els_config:get(root_uri))),
  Title = <<"Erlang LS (in ", Root/binary, ") info">>,
  Range = els_range:line(1),
  Command = els_command:make_command(Title, command(), []),
  [ els_code_lens:make_lens(Range, Command, []) ].
