%%==============================================================================
%% Application Callback Module
%%==============================================================================
-module(els_app).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(application).

%%==============================================================================
%% Exports
%%==============================================================================
%% Application Callbacks
-export([ start/2
        , stop/1
        ]).

%%==============================================================================
%% Application Callbacks
%%==============================================================================
-spec start(normal, any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  ok = application:set_env(elvis, no_output, true),
  {ok, Transport} = application:get_env(erlang_ls, transport),
  {ok, _}   = els_server:start_link(Transport),
  els_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
  ok.
