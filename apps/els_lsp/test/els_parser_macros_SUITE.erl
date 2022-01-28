-module(els_parser_macros_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

%% Test cases
-export([ callback_macro/1
        , spec_macro/1
        , type_macro/1
        , opaque_macro/1
        , wild_attrbibute_macro/1
        , type_name_macro/1
        , spec_name_macro/1
        , macro_in_application/1
        , record_def_field_macro/1
        , module_macro_as_record_name/1
        , other_macro_as_record_name/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
%%-include_lib("common_test/include/ct.hrl").
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
  Text1 = "-type ?M() :: integer() | t().",
  ?assertMatch({ok, [#{kind := type_application, id := {t, 0}},
                     #{kind := type_application, id := {erlang, integer, 0}},
                     #{kind := macro, id := 'M'}]},
               els_parser:parse(Text1)),

  %% The macro is parsed as (?M()), rather than (?M)()
  Text2 = "-type t() :: ?M().",
  ?assertMatch({ok, [#{kind := type_definition, id := {t, 0}},
                     #{kind := macro, id := {'M', 0}}]},
               els_parser:parse(Text2)),

  %% In this case the macro is parsed as expected as (?T)()
  Text3 = "-type t() :: ?M:?T().",
  ?assertMatch({ok, [#{kind := type_definition, id := {t, 0}},
                     #{kind := macro, id := 'M'},
                     #{kind := macro, id := 'T'}]},
               els_parser:parse(Text3)),
  ok.

spec_name_macro(_Config) ->
  %% Verify the parser does not crash on macros in spec function names and it
  %% still returns an unnamed spec-context and POIs from the definition body
  Text1 = "-spec ?M() -> integer() | t().",
  ?assertMatch([#{id := undefined}], parse_find_pois(Text1, spec)),
  ?assertMatch([_], parse_find_pois(Text1, type_application, {t, 0})),

  Text2 = "-spec ?MODULE:b() -> integer() | t().",
  ?assertMatch([#{id := undefined}], parse_find_pois(Text2, spec)),
  ?assertMatch([_], parse_find_pois(Text2, type_application, {t, 0})),

  Text3 = "-spec mod:?M() -> integer() | t().",
  ?assertMatch([#{id := undefined}], parse_find_pois(Text3, spec)),
  ?assertMatch([_], parse_find_pois(Text3, type_application, {t, 0})),
  ok.

macro_in_application(_Config) ->
  Text1 = "f() -> ?M:f(42).",
  ?assertMatch([#{id := 'M'}], parse_find_pois(Text1, macro)),

  Text2 = "f() -> ?M(mod):f(42).",
  ?assertMatch([#{id := {'M', 1}}], parse_find_pois(Text2, macro)),

  %% This is not an application, only a module qualifier before macro M/1
  Text3 = "f() -> mod:?M(42).",
  ?assertMatch([#{id := {'M', 1}}], parse_find_pois(Text3, macro)),

  %% Application with macro M/0 as function name
  Text4 = "f() -> mod:?M()(42).",
  ?assertMatch([#{id := {'M', 0}}], parse_find_pois(Text4, macro)),

  %% Known limitation of the current implementation,
  %% ?MODULE is handled specially, converted to a local call
  Text5 = "f() -> ?MODULE:foo().",
  ?assertMatch([], parse_find_pois(Text5, macro)),
  ?assertMatch([#{id := {foo, 0}}], parse_find_pois(Text5, application)),

  ok.

record_def_field_macro(_Config) ->
  Text1 = "-record(rec, {?M = 1}).",
  ?assertMatch({ok, [#{kind := record, id := rec},
                     #{kind := macro, id := 'M'}]},
               els_parser:parse(Text1)),

  %% typed record field
  Text2 = "-record(rec, {?M :: integer()}).",
  ?assertMatch({ok, [#{kind := record, id := rec},
                     #{kind := type_application, id := {erlang, integer, 0}},
                     #{kind := macro, id := 'M'}]},
               els_parser:parse(Text2)),
  ok.

%% Verify that record-related POIs are created with '?MODULE' name
%% also verify that no macro POI is created in these cases
module_macro_as_record_name(_Config) ->
  Text1 = "-record(?MODULE, {f = 1}).",
  ?assertMatch([#{data := #{field_list := [f]}}],
               parse_find_pois(Text1, record, '?MODULE')),
  ?assertMatch([_], parse_find_pois(Text1, record_def_field, {'?MODULE', f})),
  ?assertMatch([], parse_find_pois(Text1, macro)),

  Text2 = "-type t() :: #?MODULE{f :: integer()}.",
  ?assertMatch([_], parse_find_pois(Text2, record_expr, '?MODULE')),
  ?assertMatch([_], parse_find_pois(Text2, record_field, {'?MODULE', f})),
  ?assertMatch([], parse_find_pois(Text2, macro)),

  Text3 = "f(M) -> M#?MODULE.f.",
  ?assertMatch([_], parse_find_pois(Text3, record_expr, '?MODULE')),
  ?assertMatch([_], parse_find_pois(Text3, record_field, {'?MODULE', f})),
  ?assertMatch([], parse_find_pois(Text3, macro)),

  Text4 = "f(M) -> M#?MODULE{f = 1}.",
  ?assertMatch([_], parse_find_pois(Text4, record_expr, '?MODULE')),
  ?assertMatch([_], parse_find_pois(Text4, record_field, {'?MODULE', f})),
  ?assertMatch([], parse_find_pois(Text4, macro)),
  ok.

%% Verify macro POIs are created in record name positions
other_macro_as_record_name(_Config) ->
  Text1 = "-record(?M, {f = 1}).",
  ?assertMatch([], parse_find_pois(Text1, record)),
  ?assertMatch([], parse_find_pois(Text1, record_def_field)),
  ?assertMatch([_], parse_find_pois(Text1, macro)),

  Text2 = "-type t() :: #?M{f :: integer()}.",
  ?assertMatch([], parse_find_pois(Text2, record_expr)),
  ?assertMatch([], parse_find_pois(Text2, record_field)),
  ?assertMatch([_], parse_find_pois(Text2, macro, 'M')),

  Text3 = "f(M) -> M#?M.f.",
  ?assertMatch([], parse_find_pois(Text3, record_expr)),
  ?assertMatch([], parse_find_pois(Text3, record_field)),
  ?assertMatch([_], parse_find_pois(Text3, macro, 'M')),

  Text4 = "f(M) -> M#?M{f = 1}.",
  ?assertMatch([], parse_find_pois(Text4, record_expr)),
  ?assertMatch([], parse_find_pois(Text4, record_field)),
  ?assertMatch([_], parse_find_pois(Text4, macro, 'M')),
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
