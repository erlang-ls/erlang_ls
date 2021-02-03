-module(els_parser_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

%% Test cases
-export([ specs_location/1
        , parse_invalid_code/1
        , underscore_macro/1
        , specs_with_record/1
        , types_with_record/1
        , types_with_types/1
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
  Spec = [POI || #{id := {foo, 1}, kind := spec} = POI <- POIs],
  ?assertEqual(1, length(Spec)),
  ok.

%% Issue #170
-spec parse_invalid_code(config()) -> ok.
parse_invalid_code(_Config) ->
  Text = "foo(X) -> 16#.",
  {ok, _POIs} = els_parser:parse(Text),
  ok.

-spec underscore_macro(config()) -> ok.
underscore_macro(_Config) ->
  ?assertMatch({ok, [#{id := '_'} | _]},
               els_parser:parse("-define(_(Text), gettexter:gettext(Text)).")),
  ?assertMatch({ok, [#{id := '_'} | _]},
               els_parser:parse("-define(_, smth).")),
  ?assertMatch({ok, []},
               els_parser:parse("?_.")),
  ?assertMatch({ok, []},
               els_parser:parse("?_(ok).")),
  ok.

%% Issue #815
-spec specs_with_record(config()) -> ok.
specs_with_record(_Config) ->
  Text = "-record(bar, {a, b}). -spec foo(#bar{}) -> any().",
  {ok, POIs} = els_parser:parse(Text),
  Spec = [POI || #{id := bar, kind := record_expr} = POI <- POIs],
  ?assertEqual(1, length(Spec)),
  ok.

%% Issue #818
-spec types_with_record(config()) -> ok.
types_with_record(_Config) ->
  Text = "-record(bar, {a, b}). -type foo() :: #bar{}.",
  {ok, POIs} = els_parser:parse(Text),
  Spec = [POI || #{id := bar, kind := record_expr} = POI <- POIs],
  ?assertEqual(1, length(Spec)),
  ok.

%% Issue #818
-spec types_with_types(config()) -> ok.
types_with_types(_Config) ->
  Text = "-type bar() :: {a,b}. -type foo() :: bar().",
  {ok, POIs} = els_parser:parse(Text),
  Spec = [POI || #{id := {bar, 0}, kind := type_application} = POI <- POIs],
  ?assertEqual(1, length(Spec)),
  ok.
