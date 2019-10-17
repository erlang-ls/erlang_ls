-module(erlang_ls_references_SUITE).

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
        , function_definition/1
        , fun_local/1
        , fun_remote/1
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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, Started} = application:ensure_all_started(erlang_ls),
  [{started, Started}|Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  erlang_ls_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok.

-spec all() -> [atom()].
all() ->
  erlang_ls_test_utils:all(?MODULE).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec application_local(config()) -> ok.
application_local(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = erlang_ls_client:references(Uri, 22, 5),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {22, 3}, to => {22, 13}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {51, 7}, to => {51, 23}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec application_remote(config()) -> ok.
application_remote(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = erlang_ls_client:references(Uri, 32, 13),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {32, 3}, to => {32, 26}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {52, 8}, to => {52, 38}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec function_definition(config()) -> ok.
function_definition(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = erlang_ls_client:references(Uri, 25, 1),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {22, 3}, to => {22, 13}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {51, 7}, to => {51, 23}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec fun_local(config()) -> ok.
fun_local(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = erlang_ls_client:references(Uri, 51, 16),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {22, 3}, to => {22, 13}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {51, 7}, to => {51, 23}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec fun_remote(config()) -> ok.
fun_remote(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = erlang_ls_client:references(Uri, 52, 14),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {32, 3}, to => {32, 26}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {52, 8}, to => {52, 38}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec assert_locations([map()], [map()]) -> ok.
assert_locations(Locations, ExpectedLocations) ->
  ?assertEqual(length(ExpectedLocations), length(Locations)),
  Pairs = lists:zip(lists:sort(Locations), ExpectedLocations),
  [ begin
      #{uri := Uri, range := Range} = Location,
      #{uri := ExpectedUri, range := ExpectedRange} = Expected,
      ?assertEqual(ExpectedUri, Uri),
      ?assertEqual( erlang_ls_protocol:range(ExpectedRange)
                  , Range
                  )
    end
    || {Location, Expected} <- Pairs
  ],
  ok.
