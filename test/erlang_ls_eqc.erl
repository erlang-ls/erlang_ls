%%==============================================================================
%% QuickCheck tests
%%==============================================================================
-module(erlang_ls_eqc).

-ifdef(EQC_TESTING).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-compile(export_all).

%%==============================================================================
%% Defines
%%==============================================================================
-define(HOSTNAME, {127,0,0,1}).
-define(PORT    , 9000).

%%==============================================================================
%% Initial State
%%==============================================================================
initial_state() ->
  #{ connected => false
   , buffers   => []
   }.

%%==============================================================================
%% Commands
%%==============================================================================

%%------------------------------------------------------------------------------
%% Connect
%%------------------------------------------------------------------------------
connect() ->
  erlang_ls_client:start_link(?HOSTNAME, ?PORT).

connect_args(_S) ->
  [].

connect_pre(#{connected := Connected} = _S) ->
  not Connected.

connect_next(S, _R, _Args) ->
  S#{connected => true}.

connect_post(_S, _Args, Res) ->
  ?assertMatch({ok, _Pid}, Res),
  true.

%%------------------------------------------------------------------------------
%% Initialize
%%------------------------------------------------------------------------------
initialize() ->
  erlang_ls_client:initialize().

initialize_args(_S) ->
  [].

initialize_pre(#{connected := Connected} = _S) ->
  Connected.

initialize_next(S, _R, _Args) ->
  S.

initialize_post(_S, _Args, Res) ->
  Expected = #{ capabilities =>
                  #{ hoverProvider => false
                   , completionProvider =>
                       #{ resolveProvider => false
                        , triggerCharacters => [<<":">>, <<"#">>]
                        }
                   , textDocumentSync => 1
                   , definitionProvider => true
                   }
              },
  ?assertEqual(Expected, maps:get(result, Res)),
  true.

%%------------------------------------------------------------------------------
%% textDocument/didOpen
%%------------------------------------------------------------------------------
did_open(Uri, LanguageId, Version, Text) ->
  erlang_ls_client:did_open(Uri, LanguageId, Version, Text).

did_open_args(_S) ->
  [erlang_ls_eqc_gen:uri(), <<"erlang">>, 0, utf8()].

did_open_pre(#{connected := Connected} = _S) ->
  Connected.

did_open_next(S, _R, [Uri, _, _, _]) ->
  maps:put(buffers, maps:get(buffers, S) ++ [Uri], S).

did_open_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% textDocument/didSave
%%------------------------------------------------------------------------------
did_save(Uri) ->
  erlang_ls_client:did_save(Uri).

did_save_args(_S) ->
  [erlang_ls_eqc_gen:uri()].

did_save_pre(#{connected := Connected} = _S) ->
  Connected.

did_save_next(S, _R, _Args) ->
  S.

did_save_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% Disconnect
%%------------------------------------------------------------------------------
disconnect() ->
  erlang_ls_client:stop().

disconnect_args(_S) ->
  [].

disconnect_pre(#{connected := Connected} = _S) ->
  Connected.

disconnect_next(S, _R, _Args) ->
  S#{connected => false}.

disconnect_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%==============================================================================
%% The statem's property
%%==============================================================================
prop_main() ->
  setup(),
  ?FORALL( Cmds
         , commands(?MODULE)
         , begin
             cleanup(),
             {H, S, Res} = run_commands(Cmds),
             check_command_names( Cmds
                                , measure( length
                                         , commands_length(Cmds)
                                         , aggregate( call_features(H)
                                                    , pretty_commands( ?MODULE
                                                                     , Cmds
                                                                     , { H
                                                                       , S
                                                                       , Res},
                                                                      Res == ok)
                                                    )
                                         )
                                )
           end).

%%==============================================================================
%% Setup
%%==============================================================================
setup() ->
  application:ensure_all_started(erlang_ls),
  lager:set_loglevel(lager_console_backend, debug),
  ok.

%%==============================================================================
%% Cleanup
%%==============================================================================
cleanup() ->
  catch disconnect(),
  ok.

-endif.
