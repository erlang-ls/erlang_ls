-module(els_references_SUITE).

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
        , function_multiple_clauses/1
        , fun_local/1
        , fun_remote/1
        , export_entry/1
        , macro/1
        , included_macro/1
        , undefined_macro/1
        , module/1
        , record/1
        , record_field/1
        , included_record/1
        , included_record_field/1
        , undefined_record/1
        , undefined_record_field/1
        , type_local/1
        , type_remote/1
        , type_included/1
        , refresh_after_watched_file_deleted/1
        , refresh_after_watched_file_changed/1
        , refresh_after_watched_file_added/1
        , ignore_open_watched_file_added/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("els_core/include/els_core.hrl").

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
init_per_testcase(TestCase, Config0)
  when TestCase =:= refresh_after_watched_file_changed;
       TestCase =:= refresh_after_watched_file_deleted ->
  Config = els_test_utils:init_per_testcase(TestCase, Config0),
  PathB = ?config(watched_file_b_path, Config),
  {ok, OldContent} = file:read_file(PathB),
  [{old_content, OldContent}|Config];
init_per_testcase(TestCase, Config) ->
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config)
  when TestCase =:= refresh_after_watched_file_changed;
       TestCase =:= refresh_after_watched_file_deleted ->
  PathB = ?config(watched_file_b_path, Config),
  ok = file:write_file(PathB, ?config(old_content, Config)),
  els_test_utils:end_per_testcase(TestCase, Config);
end_per_testcase(TestCase, Config)
  when TestCase =:= refresh_after_watched_file_added;
       TestCase =:= ignore_open_watched_file_added ->
  PathB = ?config(watched_file_b_path, Config),
  ok = file:delete(filename:join(filename:dirname(PathB),
                                 "watched_file_c.erl")),
  els_test_utils:end_per_testcase(TestCase, Config);
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
                         , range => #{from => {10, 34}, to => {10, 38}}
                         }
                      , #{ uri => Uri
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

-spec function_multiple_clauses(config()) -> ok.
function_multiple_clauses(Config) ->
  Uri = ?config(hover_docs_uri, Config),
  UriCaller = ?config(hover_docs_caller_uri, Config),
  #{result := Locations} = els_client:references(Uri, 7, 1),
  ExpectedLocations = [ #{ uri => UriCaller
                         , range => #{from => {16, 3}, to => {16, 30}}
                         }
                      , #{ uri => UriCaller
                         , range => #{from => {20, 4}, to => {20, 37}}
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
                         , range => #{from => {10, 34}, to => {10, 38}}
                         }
                      , #{ uri => Uri
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
                         , range => #{from => {26, 3}, to => {26, 11}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {75, 23}, to => {75, 31}}
                         }
                      ],

  ct:comment("References for MACRO_A from usage"),
  #{result := Locations1} = els_client:references(Uri, 26, 6),
  assert_locations(Locations1, ExpectedLocations),

  ct:comment("References for MACRO_A from define"),
  #{result := Locations2} = els_client:references(Uri, 18, 12),
  assert_locations(Locations2, ExpectedLocations),

  ok.

-spec included_macro(config()) -> ok.
included_macro(Config) ->
  Uri = ?config(diagnostics_unused_includes_uri, Config),
  HeaderUri = ?config(definition_h_uri, Config),

  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {14, 23}, to => {14, 54}}
                         }
                      ],

  ct:comment("References for MACRO_FOR_TRANSITIVE_INCLUSION from usage"),
  #{result := Locations} = els_client:references(Uri, 14, 30),
  ct:comment("References for MACRO_FOR_TRANSITIVE_INCLUSION from define"),
  #{result := Locations} = els_client:references(HeaderUri, 1, 20),
  assert_locations(Locations, ExpectedLocations),

  ok.

-spec undefined_macro(config()) -> ok.
undefined_macro(Config) ->
  Uri = ?config(code_navigation_undefined_uri, Config),

  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {6, 28}, to => {6, 40}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {8, 29}, to => {8, 41}}
                         }
                      ],

  ct:comment("References for UNDEF_MACRO from usage"),
  #{result := Locations1} = els_client:references(Uri, 6, 30),
  assert_locations(Locations1, ExpectedLocations),
  ok.

-spec module(config()) -> ok.
module(Config) ->
  Uri = ?config(code_navigation_extra_uri, Config),
  #{result := Locations} = els_client:references(Uri, 1, 12),
  LocUri = ?config(code_navigation_uri, Config),
  ExpectedLocations = [ #{ uri => LocUri
                         , range => #{from => {10, 34}, to => {10, 38}}
                         }
                      , #{ uri => LocUri
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
                         , range => #{from => {23, 3}, to => {23, 12}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {33, 7}, to => {33, 16}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {34, 9}, to => {34, 18}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {34, 34}, to => {34, 43}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {99, 8}, to => {99, 17}}
                         }
                      ],

  ct:comment("Find references record_a from a usage"),
  #{result := Locations} = els_client:references(Uri, 23, 3),
  ct:comment("Find references record_a from an access"),
  #{result := Locations} = els_client:references(Uri, 34, 15),
  ct:comment("Find references record_a from beginning of definition"),
  #{result := Locations} = els_client:references(Uri, 16, 9),
  ct:comment("Find references record_a from end of definition"),
  #{result := Locations} = els_client:references(Uri, 16, 17),

  assert_locations(Locations, ExpectedLocations),

  ct:comment("Check limits of record_a"),
  #{result := null} = els_client:references(Uri, 16, 8),
  #{result := null} = els_client:references(Uri, 16, 18),

  ok.

-spec record_field(config()) -> ok.
record_field(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {33, 18}, to => {33, 25}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {34, 19}, to => {34, 26}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {34, 44}, to => {34, 51}}
                         }
                      ],

  ct:comment("Find references field_a from a usage"),
  #{result := Locations} = els_client:references(Uri, 33, 18),
  ct:comment("Find references field_a from an access"),
  #{result := Locations} = els_client:references(Uri, 34, 19),
  ct:comment("Find references field_a from beginning of definition"),
  #{result := Locations} = els_client:references(Uri, 16, 20),
  ct:comment("Find references field_a from end of definition"),
  #{result := Locations} = els_client:references(Uri, 16, 27),

  assert_locations(Locations, ExpectedLocations),

  ct:comment("Check limits of field_a"),
  #{result := null} = els_client:references(Uri, 16, 19),
  #{result := null} = els_client:references(Uri, 16, 28),

  ok.

-spec included_record(config()) -> ok.
included_record(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  HeaderUri = ?config(code_navigation_h_uri, Config),

  ExpectedRecordLocations =
    [ #{ uri => Uri
       , range => #{from => {52, 41}, to => {52, 59}}
       }
    , #{ uri => Uri
       , range => #{from => {53, 23}, to => {53, 41}}
       }
    , #{ uri => Uri
       , range => #{from => {75, 4}, to => {75, 22}}
       }
    ],
  ct:comment("Find references of included_record_a from a usage"),
  #{result := RecordLocations} = els_client:references(Uri, 53, 30),
  ct:comment("Find references of included_record_a from definition"),
  #{result := RecordLocations} = els_client:references(HeaderUri, 1, 10),
  assert_locations(RecordLocations, ExpectedRecordLocations),

  ok.

-spec included_record_field(config()) -> ok.
included_record_field(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  HeaderUri = ?config(code_navigation_h_uri, Config),

  ExpectedFieldLocations =
    [ #{ uri => Uri
       , range => #{from => {53, 42}, to => {53, 58}}
       }
    ],
  ct:comment("Find references of included_field_a from a usage"),
  #{result := FieldLocations} = els_client:references(Uri, 53, 45),
  ct:comment("Find references of included_field_a from definition"),
  #{result := FieldLocations} = els_client:references(HeaderUri, 1, 30),
  assert_locations(FieldLocations, ExpectedFieldLocations),

  ok.

-spec undefined_record(config()) -> ok.
undefined_record(Config) ->
  Uri = ?config(code_navigation_undefined_uri, Config),

  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {6, 3}, to => {6, 13}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {8, 4}, to => {8, 14}}
                         }
                      ],

  ct:comment("References for undef_rec from usage"),
  #{result := Locations1} = els_client:references(Uri, 6, 10),
  assert_locations(Locations1, ExpectedLocations),
  ok.

-spec undefined_record_field(config()) -> ok.
undefined_record_field(Config) ->
  Uri = ?config(code_navigation_undefined_uri, Config),

  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {6, 14}, to => {6, 25}}
                         }
                      , #{ uri => Uri
                         , range => #{from => {8, 15}, to => {8, 26}}
                         }
                      ],

  ct:comment("References for undef_field from usage"),
  #{result := Locations1} = els_client:references(Uri, 6, 20),
  assert_locations(Locations1, ExpectedLocations),
  ok.


-spec type_local(config()) -> ok.
type_local(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  ExpectedLocations = [ #{ uri => Uri
                         , range => #{from => {55, 23}, to => {55, 29}}
                         }
                      ],

  ct:comment("Find references type_a from its definition"),
  #{result := Locations} = els_client:references(Uri, 37, 9),
  ct:comment("Find references type_a from a usage"),
  #{result := Locations} = els_client:references(Uri, 55, 23),
  ct:comment("Find references type_a from beginning of definition"),
  #{result := Locations} = els_client:references(Uri, 37, 7),
  ct:comment("Find references type_a from end of definition"),
  #{result := Locations} = els_client:references(Uri, 37, 12),

  assert_locations(Locations, ExpectedLocations),

  ok.

-spec type_remote(config()) -> ok.
type_remote(Config) ->
  UriTypes = ?config(code_navigation_types_uri, Config),
  Uri = ?config(code_navigation_extra_uri, Config),
  ExpectedLocations = [ %% local reference from another type definition
                        #{ uri   => UriTypes
                         , range => #{from => {11, 24}, to => {11, 30}}
                         }
                        %% remote reference from a spec
                      , #{ uri   => Uri
                         , range => #{from => {11, 38}, to => {11, 66}}
                         }
                      ],

  ct:comment("Find references for type_a from a remote usage"),
  #{result := Locations} = els_client:references(Uri, 11, 45),
  ct:comment("Find references type_a from definition"),
  #{result := Locations} = els_client:references(UriTypes, 3, 8),

  assert_locations(Locations, ExpectedLocations),

  ok.

-spec type_included(config()) -> ok.
type_included(Config) ->
  UriTypes = ?config(code_navigation_types_uri, Config),
  UriHeader = ?config(definition_h_uri, Config),

  ExpectedLocations = [ #{ uri   => UriTypes
                         , range => #{from => {15, 24}, to => {15, 30}}
                         }
                      ],
  ct:comment("Find references for type_b from a remote usage"),
  #{result := Locations} = els_client:references(UriTypes, 15, 25),
  ct:comment("Find references for type_b from definition"),
  #{result := Locations} = els_client:references(UriHeader, 2, 7),
  assert_locations(Locations, ExpectedLocations),
  ok.

-spec refresh_after_watched_file_deleted(config()) -> ok.
refresh_after_watched_file_deleted(Config) ->
  %% Before
  UriA = ?config(watched_file_a_uri, Config),
  UriB = ?config(watched_file_b_uri, Config),
  PathB = ?config(watched_file_b_path, Config),
  ExpectedLocationsBefore = [ #{ uri   => UriB
                               , range => #{from => {6, 3}, to => {6, 22}}
                               }
                            ],
  #{result := LocationsBefore} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsBefore, ExpectedLocationsBefore),
  %% Delete (Simulate a checkout, rebase or similar)
  ok = file:delete(PathB),
  els_client:did_change_watched_files([{UriB, ?FILE_CHANGE_TYPE_DELETED}]),
  %% After
  #{result := null} = els_client:references(UriA, 5, 2),
  ok.

-spec refresh_after_watched_file_changed(config()) -> ok.
refresh_after_watched_file_changed(Config) ->
  %% Before
  UriA = ?config(watched_file_a_uri, Config),
  UriB = ?config(watched_file_b_uri, Config),
  PathB = ?config(watched_file_b_path, Config),
  ExpectedLocationsBefore = [ #{ uri   => UriB
                               , range => #{from => {6, 3}, to => {6, 22}}
                               }
                            ],
  #{result := LocationsBefore} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsBefore, ExpectedLocationsBefore),
  %% Edit (Simulate a checkout, rebase or similar)
  NewContent = re:replace(?config(old_content, Config),
                          "watched_file_a:main()",
                          "watched_file_a:main(), watched_file_a:main()"),
  ok = file:write_file(PathB, NewContent),
  els_client:did_change_watched_files([{UriB, ?FILE_CHANGE_TYPE_CHANGED}]),
  %% After
  ExpectedLocationsAfter = [ #{ uri   => UriB
                              , range => #{from => {6, 3}, to => {6, 22}}
                              }
                           , #{ uri   => UriB
                              , range => #{from => {6, 26}, to => {6, 45}}
                              }
                           ],
  #{result := LocationsAfter} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsAfter, ExpectedLocationsAfter),
  ok.

-spec refresh_after_watched_file_added(config()) -> ok.
refresh_after_watched_file_added(Config) ->
  %% Before
  UriA = ?config(watched_file_a_uri, Config),
  UriB = ?config(watched_file_b_uri, Config),
  PathB = ?config(watched_file_b_path, Config),
  ExpectedLocationsBefore = [ #{ uri   => UriB
                               , range => #{from => {6, 3}, to => {6, 22}}
                               }
                            ],
  #{result := LocationsBefore} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsBefore, ExpectedLocationsBefore),
  %% Add (Simulate a checkout, rebase or similar)
  DataDir = ?config(data_dir, Config),
  PathC = filename:join([DataDir, "watched_file_c.erl"]),
  NewPathC = filename:join(filename:dirname(PathB), "watched_file_c.erl"),
  NewUriC = els_uri:uri(NewPathC),
  {ok, _} = file:copy(PathC, NewPathC),
  els_client:did_change_watched_files([{NewUriC, ?FILE_CHANGE_TYPE_CREATED}]),
  %% After
  ExpectedLocationsAfter = [ #{ uri   => NewUriC
                              , range => #{from => {6, 3}, to => {6, 22}}
                              }
                           , #{ uri   => UriB
                              , range => #{from => {6, 3}, to => {6, 22}}
                              }
                           ],
  #{result := LocationsAfter} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsAfter, ExpectedLocationsAfter),
  ok.

-spec ignore_open_watched_file_added(config()) -> ok.
ignore_open_watched_file_added(Config) ->
  %% Before
  UriA = ?config(watched_file_a_uri, Config),
  UriB = ?config(watched_file_b_uri, Config),
  PathB = ?config(watched_file_b_path, Config),
  ExpectedLocationsBefore = [ #{ uri   => UriB
                               , range => #{from => {6, 3}, to => {6, 22}}
                               }
                            ],
  #{result := LocationsBefore} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsBefore, ExpectedLocationsBefore),
  %% Add (Simulate a checkout, rebase or similar)
  DataDir = ?config(data_dir, Config),
  PathC = filename:join([DataDir, "watched_file_c.erl"]),
  NewPathC = filename:join(filename:dirname(PathB), "watched_file_c.erl"),
  NewUriC = els_uri:uri(NewPathC),
  {ok, _} = file:copy(PathC, NewPathC),
  %% Open file, did_change_watched_files requests should be ignored
  els_client:did_open(NewUriC, erlang, 1, <<"dummy">>),
  els_client:did_change_watched_files([{NewUriC, ?FILE_CHANGE_TYPE_CREATED}]),
  %% After
  ExpectedLocationsOpen = [ #{ uri   => UriB
                             , range => #{from => {6, 3}, to => {6, 22}}
                             }
                          ],
  #{result := LocationsOpen} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsOpen, ExpectedLocationsOpen),
  %% Close file, did_change_watched_files requests should be resumed
  els_client:did_close(NewUriC),
  els_client:did_change_watched_files([{NewUriC, ?FILE_CHANGE_TYPE_CREATED}]),
  %% After
  ExpectedLocationsClose = [ #{ uri   => NewUriC
                              , range => #{from => {6, 3}, to => {6, 22}}
                              }
                           , #{ uri   => UriB
                              , range => #{from => {6, 3}, to => {6, 22}}
                              }
                           ],
  #{result := LocationsClose} = els_client:references(UriA, 5, 2),
  assert_locations(LocationsClose, ExpectedLocationsClose),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec assert_locations([map()], [map()]) -> ok.
assert_locations(Locations, ExpectedLocations) ->
  ?assertEqual(length(ExpectedLocations),
               length(Locations),
               { {expected, ExpectedLocations}
               , {actual, Locations}
               }
              ),
  Pairs = lists:zip(sort_locations(Locations), ExpectedLocations),
  [ begin
      #{range := Range} = Location,
      #{uri := ExpectedUri, range := ExpectedRange} = Expected,
      ?assertMatch(#{uri := ExpectedUri}, Location),
      ?assertEqual( els_protocol:range(ExpectedRange)
                  , Range
                  )
    end
    || {Location, Expected} <- Pairs
  ],
  ok.

sort_locations(Locations) ->
  lists:sort(fun compare_locations/2, Locations).

compare_locations(#{range := R1}, #{range := R2}) ->
  #{start := #{line := L1, character := C1}} = R1,
  #{start := #{line := L2, character := C2}} = R2,
  {L1, C1} < {L2, C2}.
