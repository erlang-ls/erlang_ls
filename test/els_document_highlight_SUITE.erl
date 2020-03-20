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
        , export_entry/1
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
  ExpectedLocations = [ #{range => #{from => {22, 3}, to => {22, 13}}}
                      , #{range => #{from => {51, 7}, to => {51, 23}}}
                      ],
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
    %% TODO:AZ: I would expect 25,1 to 25,10 to also show up
  ExpectedLocations = [ #{range => #{from => {22, 3}, to => {22, 13}}}
                      , #{range => #{from => {51, 7}, to => {51, 23}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

-spec fun_local(config()) -> ok.
fun_local(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 51, 16),
  ExpectedLocations = [ #{range => #{from => {22, 3}, to => {22, 13}}}
                      , #{range => #{from => {51, 7}, to => {51, 23}}}
                      ],
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

-spec export_entry(config()) -> ok.
export_entry(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:document_highlight(Uri, 5, 25),
  %% TODO:AZ: I would expect to also see 5,39 to 5,48
  ExpectedLocations = [ #{range => #{from => {22, 3}, to => {22, 13}}}
                      , #{range => #{from => {51, 7}, to => {51, 23}}}
                      ],
  assert_locations(ExpectedLocations, Locations),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec assert_locations([map()], [map()]) -> ok.
assert_locations(ExpectedLocations, Locations) ->
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
