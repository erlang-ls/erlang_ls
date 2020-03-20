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
  #{ commands => [ els_command:with_prefix(<<"replace-lines">>)
                 , els_command:with_prefix(<<"server-info">>)] }.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({workspace_executecommand, Params}, State) ->
  #{ <<"command">> := PrefixedCommand } = Params,
  Arguments = maps:get(<<"arguments">>, Params, []),
  Result = execute_command( els_command:without_prefix(PrefixedCommand)
                          , Arguments),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
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
execute_command(<<"server-info">>, _Arguments) ->
  {ok, Version} = application:get_key(?APP, vsn),
  BinVersion = list_to_binary(Version),
  Root = filename:basename(els_uri:path(els_config:get(root_uri))),
  ConfigPath = case els_config:get(config_path) of
                 undefined -> <<"undefined">>;
                 Path -> list_to_binary(Path)
               end,
  Message = <<"Erlang LS (in ", Root/binary, "), version: "
             , BinVersion/binary
             , ", config from "
             , ConfigPath/binary
            >>,
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => Message
                                }),
  [];
execute_command(Command, Arguments) ->
  lager:info("Unsupported command: [Command=~p] [Arguments=~p]"
            , [Command, Arguments]),
  [].
