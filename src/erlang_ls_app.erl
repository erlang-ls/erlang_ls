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
  %% TODO: Optionally configure number of workers from args
  Workers = application:get_env(erlang_ls_app, indexers, 10),
  {ok, _Pool} = wpool:start_sup_pool(indexers, [ {workers, Workers} ]),
  {ok, Transport} = application:get_env(erlang_ls, transport),
  {ok, _}   = erlang_ls_server:start_link(Transport),
  erlang_ls_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
  ok.
