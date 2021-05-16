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
        , record_def_with_types/1
        , record_def_with_record_type/1
        , callback_recursive/1
        , specs_recursive/1
        , types_recursive/1
        , opaque_recursive/1
        , record_def_recursive/1
        , callback_macro/1
        , spec_macro/1
        , type_macro/1
        , opaque_macro/1
        , wild_attrbibute_macro/1
        , type_name_macro/1
        , spec_name_macro/1
        , unicode_clause_pattern/1
        , latin1_source_code/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("els_lsp.hrl").

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
  ?assertMatch([_], parse_find_pois(Text, spec, {foo, 1})),
  ok.

%% Issue #170
-spec parse_invalid_code(config()) -> ok.
parse_invalid_code(_Config) ->
  Text = "foo(X) -> 16#.",
  {ok, _POIs} = els_parser:parse(Text),
  ok.

-spec underscore_macro(config()) -> ok.
underscore_macro(_Config) ->
  ?assertMatch({ok, [#{id := {'_', 1}, kind := define} | _]},
               els_parser:parse("-define(_(Text), gettexter:gettext(Text)).")),
  ?assertMatch({ok, [#{id := '_', kind := define} | _]},
               els_parser:parse("-define(_, smth).")),
  ?assertMatch({ok, [#{id := '_', kind := macro}]},
               els_parser:parse("?_.")),
  ?assertMatch({ok, [#{id := {'_', 1}, kind := macro} | _]},
               els_parser:parse("?_(ok).")),
  ok.

%% Issue #815
-spec specs_with_record(config()) -> ok.
specs_with_record(_Config) ->
  Text = "-record(bar, {a, b}). -spec foo(#bar{}) -> any().",
  ?assertMatch([_], parse_find_pois(Text, record_expr, bar)),
  ok.

%% Issue #818
-spec types_with_record(config()) -> ok.
types_with_record(_Config) ->
  Text1 = "-record(bar, {a, b}). -type foo() :: #bar{}.",
  ?assertMatch([_], parse_find_pois(Text1, record_expr, bar)),

  Text2 = "-record(bar, {a, b}). -type foo() :: #bar{f1 :: t()}.",
  ?assertMatch([_], parse_find_pois(Text2, record_expr, bar)),
  ?assertMatch([_], parse_find_pois(Text2, record_field, {bar, f1})),
  ok.

%% Issue #818
-spec types_with_types(config()) -> ok.
types_with_types(_Config) ->
  Text = "-type bar() :: {a,b}. -type foo() :: bar().",
  ?assertMatch([_], parse_find_pois(Text, type_application, {bar, 0})),
  ok.

-spec record_def_with_types(config()) -> ok.
record_def_with_types(_Config) ->
  Text1 = "-record(r1, {f1 :: t1()}).",
  ?assertMatch([_], parse_find_pois(Text1, type_application, {t1, 0})),

  Text2 = "-record(r1, {f1 = defval :: t2()}).",
  ?assertMatch([_], parse_find_pois(Text2, type_application, {t2, 0})),
  %% No redundanct atom POIs
  ?assertMatch([#{id := defval}], parse_find_pois(Text2, atom)),

  Text3 = "-record(r1, {f1 :: t1(integer())}).",
  ?assertMatch([_], parse_find_pois(Text3, type_application, {t1, 1})),
  %% No POI for builtin types like integer()
  ?assertMatch([#{id := {t1, 1}}], parse_find_pois(Text3, type_application)),

  Text4 = "-record(r1, {f1 :: m:t1(integer())}).",
  ?assertMatch([_], parse_find_pois(Text4, type_application, {m, t1, 1})),
  %% No redundanct atom POIs
  ?assertMatch([], parse_find_pois(Text4, atom)),

  ok.

-spec record_def_with_record_type(config()) -> ok.
record_def_with_record_type(_Config) ->
  Text1 = "-record(r1, {f1 :: #r2{}}).",
  ?assertMatch([_], parse_find_pois(Text1, record_expr, r2)),
  %% No redundanct atom POIs
  ?assertMatch([], parse_find_pois(Text1, atom)),

  Text2 = "-record(r1, {f1 :: #r2{f2 :: t2()}}).",
  ?assertMatch([_], parse_find_pois(Text2, record_expr, r2)),
  ?assertMatch([_], parse_find_pois(Text2, record_field, {r2, f2})),
  %% No redundanct atom POIs
  ?assertMatch([], parse_find_pois(Text2, atom)),
  ok.

-spec callback_recursive(config()) -> ok.
callback_recursive(_Config) ->
  Text = "-callback foo(#r1{f1 :: m:t1(#r2{f2 :: t2(t3())})}) -> any().",
  assert_recursive_types(Text).

-spec specs_recursive(config()) -> ok.
specs_recursive(_Config) ->
  Text = "-spec foo(#r1{f1 :: m:t1(#r2{f2 :: t2(t3())})}) -> any().",
  assert_recursive_types(Text).

-spec types_recursive(config()) -> ok.
types_recursive(_Config) ->
  Text = "-type foo() :: #r1{f1 :: m:t1(#r2{f2 :: t2(t3())})}.",
  assert_recursive_types(Text).

-spec opaque_recursive(config()) -> ok.
opaque_recursive(_Config) ->
  Text = "-opaque foo() :: #r1{f1 :: m:t1(#r2{f2 :: t2(t3())})}.",
  assert_recursive_types(Text).

-spec record_def_recursive(config()) -> ok.
record_def_recursive(_Config) ->
  Text = "-record(foo, {field :: #r1{f1 :: m:t1(#r2{f2 :: t2(t3())})}}).",
  assert_recursive_types(Text).

assert_recursive_types(Text) ->
  ?assertMatch([#{id := r1},
                #{id := r2}],
               parse_find_pois(Text, record_expr)),
  ?assertMatch([#{id := {r1, f1}},
                #{id := {r2, f2}}],
               parse_find_pois(Text, record_field)),
  ?assertMatch([#{id := {m, t1, 1}},
                #{id := {t2, 1}},
                #{id := {t3, 0}}],
               parse_find_pois(Text, type_application)),
  ok.

-spec callback_macro(config()) -> ok.
callback_macro(_Config) ->
  Text = "-callback foo() -> ?M.",
  ?assertMatch([_], parse_find_pois(Text, callback, {foo, 0})),
  ?assertMatch([_], parse_find_pois(Text, macro, 'M')),
  ok.

-spec spec_macro(config()) -> ok.
spec_macro(_Config) ->
  Text = "-spec foo() -> ?M().",
  ?assertMatch([_], parse_find_pois(Text, spec, {foo, 0})),
  ?assertMatch([_], parse_find_pois(Text, macro, {'M', 0})),
  ok.

-spec type_macro(config()) -> ok.
type_macro(_Config) ->
  Text = "-type t() :: ?M(a, b, c).",
  ?assertMatch([_], parse_find_pois(Text, type_definition, {t, 0})),
  ?assertMatch([_], parse_find_pois(Text, macro, {'M', 3})),
  ok.

-spec opaque_macro(config()) -> ok.
opaque_macro(_Config) ->
  Text = "-opaque o() :: ?M(a, b).",
  ?assertMatch([_], parse_find_pois(Text, type_definition, {o, 0})),
  ?assertMatch([_], parse_find_pois(Text, macro, {'M', 2})),
  ok.

-spec wild_attrbibute_macro(config()) -> ok.
wild_attrbibute_macro(_Config) ->
  %% This is parsed as -(?M(foo)), rather than -(?M)(foo)
  Text = "-?M(foo).",
  ?assertMatch([_], parse_find_pois(Text, macro, {'M', 1})),
  ?assertMatch([_], parse_find_pois(Text, atom, foo)),
  ok.

type_name_macro(_Config) ->
  %% Currently els_dodger cannot parse this type definition
  %% Verify this does not prevent parsing following forms
  Text = "-type ?M() -> integer() | t(). -spec f() -> any().",
  ?assertMatch({ok, [#{kind := spec, id := {f, 0}}]}, els_parser:parse(Text)),
  ok.

spec_name_macro(_Config) ->
  %% Verify the parser does not crash on macros in spec function names and it
  %% still returns an unnamed spec-context and POIs from the definition body
  Text1 = "-spec ?M() -> integer() | t().",
  ?assertMatch([#{id := undefined}], parse_find_pois(Text1, spec)),
  ?assertMatch([_], parse_find_pois(Text1, type_application, {t, 0})),

  Text2 = "-spec ?MODULE:b() -> integer() | t().",
  ?assertMatch([#{id := undefined}], parse_find_pois(Text2, spec)),
  %% TODO: Update erlfmt, a later version can parse this
  %%?assertMatch([_], parse_find_pois(Text2, type_application, {t, 0})),
  ok.

-spec unicode_clause_pattern(config()) -> ok.
unicode_clause_pattern(_Config) ->
  %% From OTP compiler's bs_utf_SUITE.erl
  Text = "match_literal(<<\"Мастер и Маргарита\"/utf8>>) -> mm_utf8.",
  ?assertMatch([#{data := <<"(<<\"", _/binary>>}],
               parse_find_pois(Text, function_clause, {match_literal, 1, 1})),
  ok.

%% Issue #306, PR #592
-spec latin1_source_code(config()) -> ok.
latin1_source_code(_Config) ->
  Text = lists:flatten(["f(\"", 200, "\") -> 200. %% ", 200]),
  ?assertMatch([#{data := <<"(\"È\") "/utf8>>}],
               parse_find_pois(Text, function_clause, {f, 1, 1})),
  ok.

%%==============================================================================
%% Helper functions
%%==============================================================================
-spec parse_find_pois(string(), poi_kind()) -> [poi()].
parse_find_pois(Text, Kind) ->
  {ok, POIs} = els_parser:parse(Text),
  SortedPOIs = els_poi:sort(POIs),
  [POI || #{kind := Kind1} = POI <- SortedPOIs, Kind1 =:= Kind].

-spec parse_find_pois(string(), poi_kind(), poi_id()) -> [poi()].
parse_find_pois(Text, Kind, Id) ->
  [POI || #{id := Id1} = POI <- parse_find_pois(Text, Kind), Id1 =:= Id].
