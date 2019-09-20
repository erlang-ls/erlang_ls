%%==============================================================================
%% PropEr tests
%%==============================================================================
-module(prop_statem).


%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(proper_statem).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper_contrib/include/proper_contrib_statem.hrl").

%%==============================================================================
%% Exports
%%==============================================================================
-compile(export_all).
-compile(nowarn_export_all).

%%==============================================================================
%% Defines
%%==============================================================================
-define(HOSTNAME, {127,0,0,1}).
-define(PORT    , 10000).

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
initialize(RootUri, InitOptions) ->
  erlang_ls_client:initialize(RootUri, InitOptions).

initialize_args(_S) ->
  [ erlang_ls_proper_gen:root_uri()
  , erlang_ls_proper_gen:init_options()
  ].

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
  [erlang_ls_proper_gen:uri(), <<"erlang">>, 0, utf8()].

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
  [erlang_ls_proper_gen:uri()].

did_save_pre(#{connected := Connected} = _S) ->
  Connected.

did_save_next(S, _R, _Args) ->
  S.

did_save_post(_S, _Args, Res) ->
  ?assertEqual(ok, Res),
  true.

%%------------------------------------------------------------------------------
%% textDocument/didClose
%%------------------------------------------------------------------------------
did_close(Uri) ->
  erlang_ls_client:did_close(Uri).

did_close_args(_S) ->
  [erlang_ls_proper_gen:uri()].

did_close_pre(#{connected := Connected} = _S) ->
  Connected.

did_close_next(S, _R, _Args) ->
  S.

did_close_post(_S, _Args, Res) ->
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
  Config = #{ setup_fun    => fun setup/0
            , teardown_fun => fun teardown/1
            , cleanup_fun  => fun cleanup/0
            },
  proper_contrib_statem:run(?MODULE, Config).

%%==============================================================================
%% Setup
%%==============================================================================
setup() ->
  meck:new(erlang_ls_compiler_diagnostics, [no_link, passthrough]),
  meck:new(erlang_ls_dialyzer_diagnostics, [no_link, passthrough]),
  meck:expect(erlang_ls_compiler_diagnostics, diagnostics, 1, []),
  meck:expect(erlang_ls_dialyzer_diagnostics, diagnostics, 1, []),
  application:ensure_all_started(erlang_ls),
  lager:set_loglevel(lager_console_backend, warning),
  ok.

%%==============================================================================
%% Teardown
%%==============================================================================
teardown(_) ->
  meck:unload(erlang_ls_compiler_diagnostics),
  meck:unload(erlang_ls_dialyzer_diagnostics),
  ok.

%%==============================================================================
%% Cleanup
%%==============================================================================
cleanup() ->
  catch disconnect(),
  ok.
