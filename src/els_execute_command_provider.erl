-module(els_execute_command_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        , add_server_prefix/1
        , strip_server_prefix/1
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{ commands => [add_server_prefix(<<"replace-lines">>)] }.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({workspace_executecommand, Params}, State) ->
  #{ <<"command">> := PrefixedCommand } = Params,
  Arguments = maps:get(<<"arguments">>, Params, []),
  Result = execute_command(strip_server_prefix(PrefixedCommand), Arguments),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec execute_command(binary(), [any()]) -> [map()].
execute_command(<<"replace-lines">>
               , [#{ <<"uri">>   := Uri
                   , <<"lines">> := Lines
                   , <<"from">>  := LineFrom
                   , <<"to">>    := LineTo }] = _Arguments) ->
  Method = <<"workspace/applyEdit">>,
  Params = #{ edit =>
                  els_text_edit:edit_replace_text(Uri, Lines, LineFrom, LineTo)
            },
  els_server:send_request(Method, Params),
  [];
execute_command(Command, Arguments) ->
  lager:info("Unsupported command: [Command=~p] [Arguments=~p]"
            , [Command, Arguments]),
  [].


%% @doc Strip a server-unique prefix from a command.
-spec strip_server_prefix(binary()) -> binary().
strip_server_prefix(PrefixedCommand) ->
  [_, Command] = binary:split(PrefixedCommand, <<":">>),
  Command.

%% @doc Add a server-unique prefix to a command.
-spec add_server_prefix(binary()) -> binary().
add_server_prefix(Command) ->
  Prefix = server_prefix(),
  <<Prefix/binary, ":", Command/binary>>.

%% @doc Generate a prefix unique to this running erlang_ls server.  This is
%% needed because some clients have a global namespace for all registered
%% commands, and we need to be able to run multiple erlang_ls instances at the
%% same time against a single client.
-spec server_prefix() -> binary().
server_prefix() ->
   els_utils:to_binary(os:getpid()).
