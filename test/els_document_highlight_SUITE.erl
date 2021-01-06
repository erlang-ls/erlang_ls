-module(els_document_highlight_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
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
        , record/1
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
  [{group, tcp}, {group, stdio}].

-spec groups() -> [atom()].
groups() ->
  els_test_utils:groups(?MODULE).

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
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec record(config()) -> ok.
record(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 23, 4),
  ExpectedLocations = [ #{range => #{from => {23, 4}, to => {23, 12}}}
                      , #{range => #{from => {33, 8}, to => {33, 16}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec record_access(config()) -> ok.
record_access(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 34, 10),
  ExpectedLocations = [ #{range => #{from => {34, 10}, to => {34, 26}}}
                      , #{range => #{from => {34, 35}, to => {34, 51}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec record_field(config()) -> ok.
record_field(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 16, 23),
  ExpectedLocations = [ #{range => #{from => {33, 18}, to => {33, 25}}}
                      , #{range => #{from => {16, 20}, to => {16, 27}}}
                      %% TODO record access not highlighted
                      %%, #{range => #{from => {34, 19}, to => {34, 26}}}
                      %%, #{range => #{from => {34, 44}, to => {34, 51}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec export(config()) -> ok.
export(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 5, 5),
  ExpectedLocations = [ #{range => #{from => {5, 1}, to => {5, 68}}}
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
  ExpectedLocations = [ #{range => #{from => {37, 7}, to => {37, 13}}}
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
  ExpectedLocations = [ #{range => #{from => {7, 9}, to => {7, 22}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec macro(config()) -> ok.
macro(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 26, 6),

  %% Should include this range (macro definition)
  %% but does not right now
  %%  #{ range => #{ from => {18, 9}, to => {18, 16} } },

  ExpectedLocations = [ #{range => #{from => {26, 3}, to => {26, 11}}}
                      , #{range => #{from => {75, 23}, to => {75, 31}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec spec(config()) -> ok.
spec(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 55, 11),
  ExpectedLocations = [ #{range => #{from => {55, 1}, to => {55, 64}}}
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
  ct:log("ExpectedLocations: ~p~nLocations: ~p~n",
         [ExpectedLocations, lists:sort(Locations)]),
  ?assertEqual(length(ExpectedLocations), length(Locations)),
  Pairs = lists:zip(lists:sort(Locations), ExpectedLocations),
  [ begin
      #{range := Range} = Location,
      #{range := ExpectedRange} = Expected,
      ?assertEqual( els_protocol:range(ExpectedRange)
                  , Range
                  )
    end
    || {Location, Expected} <- Pairs
  ],
  ok.

-spec expected_definitions() -> [map()].
expected_definitions() ->
  [ #{range => #{from => {25, 1}, to => {25, 11}}}
  , #{range => #{from => {22, 3}, to => {22, 13}}}
  , #{range => #{from => {51, 7}, to => {51, 23}}}
  , #{range => #{from => {5, 25}, to => {5, 37}}}
  ].
