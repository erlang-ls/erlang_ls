-module(els_references_SUITE).

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
        , function_definition/1
        , fun_local/1
        , fun_remote/1
        , export_entry/1
        , macro/1
        , module/1
        , record/1
        , purge_references/1
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

-spec all() -> [{group, atom()}].
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
  #{result := Locations} = els_client:references(Uri, 22, 5),
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
  #{result := Locations} = els_client:references(Uri, 32, 13),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {32, 3}, to => {32, 27}}
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
  #{result := Locations} = els_client:references(Uri, 25, 1),
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
  #{result := Locations} = els_client:references(Uri, 51, 16),
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
  #{result := Locations} = els_client:references(Uri, 52, 14),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {32, 3}, to => {32, 27}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {52, 8}, to => {52, 38}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec export_entry(config()) -> ok.
export_entry(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Locations} = els_client:references(Uri, 5, 25),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {22, 3}, to => {22, 13}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {51, 7}, to => {51, 23}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec macro(config()) -> ok.
macro(Config) ->
  Uri = ?config(code_navigation_uri, Config),

  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {26, 3}, to => {26, 10}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {75, 23}, to => {75, 30}}
                         }
                      ],

  ct:comment("References for MACRO_A from usage"),
  #{result := Locations1} = els_client:references(Uri, 26, 6),
  assert_locations(Locations1, ExpectedLocations),

  ct:comment("References for MACRO_A from define"),
  #{result := Locations2} = els_client:references(Uri, 18, 12),
  assert_locations(Locations2, ExpectedLocations),

  ok.

-spec module(config()) -> ok.
module(Config) ->
  Uri = ?config(code_navigation_extra_uri, Config),
  #{result := Locations} = els_client:references(Uri, 1, 12),
  LocUri = ?config(code_navigation_uri, Config),
  ExpectedLocations = [ #{ uri => LocUri
                         , range => #{from => {32, 3}, to => {32, 27}}
                         }
                      , #{ uri => LocUri
                         , range => #{from => {52, 8}, to => {52, 38}}
                         }
                      ],
  assert_locations(Locations, ExpectedLocations),
  ok.


-spec record(config()) -> ok.
record(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {23, 2}, to => {23, 11}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {33, 6}, to => {33, 15}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {34, 7}, to => {34, 25}}
                         }
                      ],

  ct:comment("Find references record_a from a usage"),
  #{result := Locations} = els_client:references(Uri, 23, 4),
  ct:comment("Find references record_a from a field usage"),
  #{result := Locations} = els_client:references(Uri, 34, 22),
  ct:comment("Find references record_a from beginning of definition"),
  #{result := Locations} = els_client:references(Uri, 16, 9),
  ct:comment("Find references record_a from end of definition"),
  #{result := Locations} = els_client:references(Uri, 16, 16),

  assert_locations(Locations, ExpectedLocations),

  ct:comment("Check limits of record_a"),
  #{result := null} = els_client:references(Uri, 16, 8),
  #{result := null} = els_client:references(Uri, 16, 18),

  ok.

%% Issue #245
-spec purge_references(config()) -> ok.
purge_references(_Config) ->
  els_db:clear_tables(),
  Uri   = <<"file://tmp/foo.erl">>,
  Text0 = "-spec foo(integer()) -> ok.\nfoo(_X) -> ok.\nbar() -> foo(1).",
  Text1 = "\n-spec foo(integer()) -> ok.\nfoo(_X)-> ok.\nbar() -> foo(1).",
  Doc0  = els_dt_document:new(Uri, Text0),
  Doc1  = els_dt_document:new(Uri, Text1),

  ok = els_indexer:index(Uri, Text0, 'deep'),
  ?assertEqual({ok, [Doc0]}, els_dt_document:lookup(Uri)),
  ?assertEqual({ok, [#{ id    => {foo, foo, 1}
                      , range => #{from => {3, 10}, to => {3, 13}}
                      , uri   => <<"file://tmp/foo.erl">>
                      }]}
              , els_dt_references:find_all()
              ),

  ok = els_indexer:index(Uri, Text1, 'deep'),
  ?assertEqual({ok, [Doc1]}, els_dt_document:lookup(Uri)),
  ?assertEqual({ok, [#{ id    => {foo, foo, 1}
                      , range => #{from => {4, 10}, to => {4, 13}}
                      , uri   => <<"file://tmp/foo.erl">>
                      }]}
              , els_dt_references:find_all()
              ),
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
      ?assertEqual( els_protocol:range(ExpectedRange)
                  , Range
                  )
    end
    || {Location, Expected} <- Pairs
  ],
  ok.
