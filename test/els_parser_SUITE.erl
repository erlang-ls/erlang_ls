-module(els_parser_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

%% Test cases
-export([ specs_location/1
        , parse_invalid_code/1
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
all() -> els_test_utils:all(?MODULE).

%%==============================================================================
%% Testcases
%%==============================================================================
%% Issue #120
-spec specs_location(config()) -> ok.
specs_location(_Config) ->
  Text = "-spec foo(integer()) -> any(); (atom()) -> pid().",
  {ok, POIs} = els_parser:parse(Text),
  Spec = [POI || #{data := {{foo, 1}, _}, kind := spec} = POI <- POIs],
  ?assertEqual(1, length(Spec)),
  ok.

%% Issue #170
-spec parse_invalid_code(config()) -> ok.
parse_invalid_code(_Config) ->
  Text = "foo(X) -> 16#.",
  {ok, _POIs} = els_parser:parse(Text),
  ok.
