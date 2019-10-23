-module(erlang_ls_parser_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

%% Test cases
-export([ specs_location/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) -> ok.

-spec all() -> [atom()].
all() -> erlang_ls_test_utils:all(?MODULE).

%%==============================================================================
%% Testcases
%%==============================================================================
%% Issue #120
-spec specs_location(config()) -> ok.
specs_location(_Config) ->
  Text = "-spec foo(integer()) -> any(); (atom()) -> pid().",
  {ok, _Tree, Extra} = erlang_ls_parser:parse(Text),
  SpecLocations = maps:get(spec_locations, Extra, []),
  Locations     = proplists:get_value({foo, 1}, SpecLocations),
  ?assertEqual(4, length(Locations)),
  ok.
