-module(els_document_highlight_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ application_local/1
        , application_remote/1
        , application_imported/1
        , function_definition/1
        , fun_local/1
        , fun_remote/1
        , atom/1
        , quoted_atom/1
        , record_def/1
        , record_expr/1
        , record_access/1
        , record_field/1
        , export/1
        , export_entry/1
        , export_type_entry/1
        , import/1
        , import_entry/1
        , type/1
        , type_application/1
        , opaque/1
        , macro_define/1
        , macro/1
        , spec/1
        , behaviour/1
        , callback/1
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
-spec suite() -> [tuple()].
suite() ->
  [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
  els_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec application_local(config()) -> ok.
application_local(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 22, 5),
  ExpectedLocations = expected_definitions(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec application_remote(config()) -> ok.
application_remote(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 32, 13),
  ExpectedLocations = [ #{range => #{from => {32, 3}, to => {32, 27}}}
                      , #{range => #{from => {52, 8}, to => {52, 38}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec application_imported(config()) -> ok.
application_imported(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 35, 4),
  ExpectedLocations = [ #{range => #{from => {35, 3}, to => {35, 9}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec function_definition(config()) -> ok.
function_definition(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 25, 1),
  ExpectedLocations = expected_definitions(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec fun_local(config()) -> ok.
fun_local(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 51, 16),
  ExpectedLocations = expected_definitions(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec fun_remote(config()) -> ok.
fun_remote(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 52, 14),
  ExpectedLocations = [ #{range => #{from => {32, 3}, to => {32, 27}}}
                      , #{range => #{from => {52, 8}, to => {52, 38}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec atom(config()) -> ok.
atom(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 94, 5),
  ExpectedLocations = [ #{range => #{from => {94, 4}, to => {94, 11}}}
                      , #{range => #{from => {33, 18}, to => {33, 25}}}
                      , #{range => #{from => {34, 19}, to => {34, 26}}}
                      , #{range => #{from => {16, 20}, to => {16, 27}}}
                      , #{range => #{from => {34, 44}, to => {34, 51}}}
                      , #{range => #{from => {111, 19}, to => {111, 26}}}
                      , #{range => #{from => {113, 33}, to => {113, 40}}}
                      , #{range => #{from => {116, 14}, to => {116, 21}}}
                      , #{range => #{from => {116, 39}, to => {116, 46}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec quoted_atom(config()) -> ok.
quoted_atom(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations1} = els_client:document_highlight(Uri, 98, 1),
  ExpectedLocations1 = [ #{range => #{from => {98, 1}, to => {98, 21}}}
                       , #{range => #{from => {5, 67}, to => {5, 89}}}
                       ],
  assert_locations(ExpectedLocations1, Locations1),
  #{result := Locations2} = els_client:document_highlight(Uri, 101, 20),
  ExpectedLocations2 = [ #{range => #{from => {101, 5}, to => {101, 77}}}
                       ],
  assert_locations(ExpectedLocations2, Locations2),
  #{result := Locations3} = els_client:document_highlight(Uri, 99, 18),
  ExpectedLocations3 = [ #{range => #{from => {99, 18}, to => {99, 27}}}
                       , #{range => #{from => {16, 38}, to => {16, 47}}}
                       ],
  assert_locations(ExpectedLocations3, Locations3),
  #{result := Locations4} = els_client:document_highlight(Uri, 100, 12),
  ExpectedLocations4 = [ #{range => #{from => {100, 7}, to => {100, 43}}}
                       ],
  assert_locations(ExpectedLocations4, Locations4),
  #{result := Locations5} = els_client:document_highlight(Uri, 97, 48),
  ExpectedLocations5 = [ #{range => #{from => {97, 34}, to => {97, 68}}}
                       ],
  assert_locations(ExpectedLocations5, Locations5),
  ok.

-spec record_def(config()) -> ok.
record_def(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 16, 10),
  ExpectedLocations = record_uses(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec record_expr(config()) -> ok.
record_expr(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 23, 4),
  ExpectedLocations = record_uses(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec record_access(config()) -> ok.
record_access(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 34, 10),
  ExpectedLocations = record_uses(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec record_field(config()) -> ok.
record_field(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 16, 23),
  ExpectedLocations = [ #{range => #{from => {33, 18}, to => {33, 25}}}
                      , #{range => #{from => {16, 20}, to => {16, 27}}}
                      , #{range => #{from => {34, 19}, to => {34, 26}}}
                      , #{range => #{from => {34, 44}, to => {34, 51}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec export(config()) -> ok.
export(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 5, 5),
  ExpectedLocations = [ #{range => #{from => {5, 1}, to => {5, 108}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec export_entry(config()) -> ok.
export_entry(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 5, 25),
  ExpectedLocations = expected_definitions(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec export_type_entry(config()) -> ok.
export_type_entry(Config) ->
  Uri = ?config(code_navigation_types_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 5, 16),
  ExpectedLocations = [ #{range => #{from => {5, 16}, to => {5, 24}}}
                        %% Should also include the definition, but does not
                        %%, #{range => #{from => {3, 7}, to => {3, 13}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec import(config()) -> ok.
import(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 10, 3),
  ExpectedLocations = null,
  %% Should include this range
  %% ExpectedLocations = [ #{range => #{from => {10, 1}, to => {10, 51}}}
  %%                     ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec import_entry(config()) -> ok.
import_entry(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 10, 34),
  ExpectedLocations = [ #{range => #{from => {10, 34}, to => {10, 38}}}
                        %% Should include uses of function but does not
                        %% , #{range => #{from => {90, 3}, to => {90, 5}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec type(config()) -> ok.
type(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 37, 9),
  ExpectedLocations = [ #{range => #{from => {37, 1}, to => {37, 25}}}
                        %% Should also include the usage, but does not
                        %%, #{range => #{from => {55, 23}, to => {55, 29}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec type_application(config()) -> ok.
type_application(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 55, 57),
  ExpectedLocations = [ #{range => #{from => {55, 55}, to => {55, 62}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec opaque(config()) -> ok.
opaque(Config) ->
  Uri = ?config(code_navigation_types_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 7, 9),
  ExpectedLocations = [ #{range => #{from => {7, 1}, to => {7, 35}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec macro_define(config()) -> ok.
macro_define(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 18, 10),

  ExpectedLocations = macro_uses(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec macro(config()) -> ok.
macro(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 26, 6),

  ExpectedLocations = macro_uses(),
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec spec(config()) -> ok.
spec(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 55, 11),
  %% The entire "-spec ... ." is part of the poi range
  ExpectedLocations = [ #{range => #{from => {55, 1}, to => {55, 65}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec behaviour(config()) -> ok.
behaviour(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 3, 1),
  ExpectedLocations = [ #{range => #{from => {3, 1}, to => {3, 25}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec callback(config()) -> ok.
callback(Config) ->
  Uri = ?config(rename_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 3, 10),
  ExpectedLocations = [ #{range => #{from => {3, 1}, to => {3, 20}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec assert_locations([map()] | null, [map()] | null) -> ok.
assert_locations(null, null) ->
  ok;
assert_locations(ExpectedLocations, Locations) ->
  SortFun = fun(#{ range := #{ start := #{ line := StartLineA,
                                           character := StartCharA },
                               'end' := #{ line := EndLineA,
                                           character := EndCharA } } },
                #{ range := #{ start := #{ line := StartLineB,
                                           character := StartCharB },
                               'end' := #{ line := EndLineB,
                                           character := EndCharB } } }) ->
                    {{StartLineA, StartCharA}, {EndLineA, EndCharA}}
                        =<
                    {{StartLineB, StartCharB}, {EndLineB, EndCharB}}
            end,
  ExpectedProtoLocs = lists:sort(SortFun, protocol_ranges(ExpectedLocations)),
  SortedLocations = lists:sort(SortFun, Locations),
  ?assertEqual(length(ExpectedLocations), length(Locations)),
  Pairs = lists:zip(SortedLocations, ExpectedProtoLocs),
  [ begin
      #{range := Range} = Location,
      #{range := ExpectedRange} = Expected,
      ?assertEqual(ExpectedRange, Range)
    end
    || {Location, Expected} <- Pairs
  ],
  ok.

protocol_ranges(Locations) ->
  [ L#{range => els_protocol:range(R)}
    || L = #{range := R} <- Locations ].

-spec expected_definitions() -> [map()].
expected_definitions() ->
  [ #{range => #{from => {25, 1}, to => {25, 11}}}
  , #{range => #{from => {22, 3}, to => {22, 13}}}
  , #{range => #{from => {51, 7}, to => {51, 23}}}
  , #{range => #{from => {5, 25}, to => {5, 37}}}
  ].

-spec record_uses() -> [map()].
record_uses() ->
  [ #{range => #{from => {16, 9}, to => {16, 17}}}
  , #{range => #{from => {23, 3}, to => {23, 12}}}
  , #{range => #{from => {33, 7}, to => {33, 16}}}
  , #{range => #{from => {34, 9}, to => {34, 18}}}
  , #{range => #{from => {34, 34}, to => {34, 43}}}
  , #{range => #{from => {99, 8}, to => {99, 17}}}
  ].

-spec macro_uses() -> [map()].
macro_uses() ->
  [ #{range => #{from => {18, 9}, to => {18, 16}}}
  , #{range => #{from => {26, 3}, to => {26, 11}}}
  , #{range => #{from => {75, 23}, to => {75, 31}}}
  ].
