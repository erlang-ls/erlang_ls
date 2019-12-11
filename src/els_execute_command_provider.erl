-module(els_execute_command_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{ commands => ["foo"] }.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({workspace_executecommand, Params}, State) ->
  #{ <<"command">> := Command } = Params,
  Arguments = maps:get(<<"arguments">>, Params, []),
  Result = execute_command(Command, Arguments),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec execute_command(string(), [any()]) -> [map()].
execute_command(<<"add-lines">>, [#{ <<"uri">> := Uri
                                   , <<"lines">>   := Lines
                                   , <<"before">> := Before }] = Arguments) ->
  lager:info("execute_command:add-lines  [Arguments=~p]", [Arguments]),
  Method = <<"workspace/applyEdit">>,
  Params  = #{  edit => els_text_edit:edit_insert_text(Uri, Lines, Before)
             },
  lager:info("execute_command:add-lines  [Params=~p]", [Params]),
  els_server:send_request(Method, Params),
  [];
execute_command(Command, Arguments) ->
  lager:info("execute_command: [Command=~p] [Arguments=~p]"
            , [Command, Arguments]),
  [].
