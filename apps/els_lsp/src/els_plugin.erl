%%%=============================================================================
%%% @doc Behaviour for Erlang LS Plugins
%%% @end
%%%=============================================================================
-module(els_plugin).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("els_core/include/els_core.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type capabilities() :: #{ diagnostics => boolean() }.
-type plugin() :: atom().
-type feature() :: diagnostics.

%%==============================================================================
%% Callback Functions Definitions
%%==============================================================================
-callback name() -> binary().
-callback provides() -> capabilities().
-callback on_diagnostics_run(uri()) -> ok.
-callback init() -> ok.
-callback is_enabled() -> boolean().

-optional_callbacks([ on_diagnostics_run/1 ]).

%% TODO: Add is_supported or similar

-export([ all_plugins/0
        , enabled_plugins/0
        , enabled_plugins/1
        , provides/2
        ]).

-export([ run_diagnostics/1
        , run_diagnostics/2
        ]).

all_plugins() ->
  [els_plugin_edoc].

enabled_plugins() ->
  [Plugin || Plugin <- all_plugins(), Plugin:is_enabled()].

-spec enabled_plugins(feature()) -> [plugin()].
enabled_plugins(Feature) ->
  [Plugin || Plugin <- enabled_plugins(), provides(Plugin, Feature)].

provides(Plugin, Feature) ->
  maps:get(Feature, Plugin:provides(), false).

run_diagnostics(Uri) ->
  lists:flatten([run_diagnostics(Plugin, Uri) || Plugin <- enabled_plugins()]).

-spec run_diagnostics(plugin(), uri()) -> [pid()].
run_diagnostics(Plugin, Uri) ->
  case provides(Plugin, diagnostics) of
    true ->
      ?LOG_DEBUG("[~p] Running diagnostics. [uri=~p] ", [Plugin, Uri]),
      Module = atom_to_binary(els_uri:module(Uri), utf8),
      Source = Plugin:name(),
      Title = <<Source/binary, " (", Module/binary, ")">>,
      Config = #{ task => fun(U, _) -> Plugin:on_diagnostics_run(U) end
                , entries => [Uri]
                , title => Title
                , on_complete =>
                    fun(Diagnostics) ->
                        %% TODO: Generalize support for telemetry
                        els_diagnostics_provider:notify(Diagnostics, self())
                    end
                },
      {ok, Pid} = els_background_job:new(Config),
      [Pid];
    false ->
      []
  end.
