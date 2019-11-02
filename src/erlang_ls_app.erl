%%==============================================================================
%% Application Callback Module
%%==============================================================================
-module(erlang_ls_app).

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
  Transport = application:get_env(erlang_ls, transport, erlang_ls_tcp),
  {ok, _} = erlang_ls_server:start_link(Transport),
  {ok, _} = Transport:start_listener(),
  erlang_ls_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
  ok.
