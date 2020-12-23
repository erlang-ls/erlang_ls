-module(els_rename_SUITE).

-include("erlang_ls.hrl").

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
-export([ rename_behaviour_callback/1
        , rename_macro/1
        , rename_parametrized_macro/1
        , rename_macro_from_usage/1
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
-spec rename_behaviour_callback(config()) -> ok.
rename_behaviour_callback(Config) ->
  Uri = ?config(rename_uri, Config),
  Line = 2,
  Char = 9,
  NewName = <<"new_awesome_name">>,
  #{result := Result} = els_client:document_rename(Uri, Line, Char, NewName),
  Expected =  #{changes =>
                  #{ binary_to_atom(Uri, utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 19, line => 2}
                               , start => #{character => 10, line => 2}}}
                       ]
                   , binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 18, line => 6}
                               , start => #{character => 9, line => 6}}}
                       , #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 9, line => 9}
                               , start => #{character => 0, line => 9}}}
                       , #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 9, line => 11}
                               , start => #{character => 0, line => 11}}}
                       , #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 15, line => 8}
                               , start => #{character => 6, line => 8}}}
                       ]
                   , binary_to_atom(?config(rename_usage2_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 18, line => 6}
                               , start => #{character => 9, line => 6}}}
                       , #{ newText => NewName
                          , range =>
                             #{ 'end' => #{character => 9, line => 8}
                              , start => #{character => 0, line => 8}}}
                       ]
                   }
               },
  assert_changes(Expected, Result).

-spec rename_macro(config()) -> ok.
rename_macro(Config) ->
  Uri = ?config(rename_h_uri, Config),
  Line = 0,
  Char = 13,
  NewName = <<"NEW_AWESOME_NAME">>,
  #{result := Result} = els_client:document_rename(Uri, Line, Char, NewName),
  Expected =  #{changes =>
                  #{ binary_to_atom(Uri, utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 17, line => 0}
                               , start => #{character => 8, line => 0}}}
                       ]
                   , binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 14, line => 15}
                               , start => #{character => 4, line => 15}}}
                       , #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 26, line => 15}
                               , start => #{character => 16, line => 15}}}
                       ]
                   , binary_to_atom(?config(rename_usage2_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 27, line => 11}
                               , start => #{character => 17, line => 11}}}
                       ]
                   }
               },
  assert_changes(Expected, Result).

-spec rename_parametrized_macro(config()) -> ok.
rename_parametrized_macro(Config) ->
  Uri = ?config(rename_h_uri, Config),
  Line = 2,
  Char = 16,
  NewName = <<"NEW_AWESOME_NAME">>,
  #{result := Result} = els_client:document_rename(Uri, Line, Char, NewName),
  Expected =  #{changes =>
                  #{ binary_to_atom(Uri, utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 30, line => 2}
                               , start => #{character => 8, line => 2}}}
                       ]
                   , binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 27, line => 18}
                               , start => #{character => 4, line => 18}}}
                       , #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 55, line => 18}
                               , start => #{character => 32, line => 18}}}
                       ]
                   %%   This is currently not possible due to #805
                   %% , binary_to_atom(
                   %%     ?config(rename_usage2_uri, Config), utf8) =>
                   %%     [ #{ newText => NewName
                   %%        , range =>
                   %%            #{ 'end' => #{character => 52, line => 14}
                   %%             , start => #{character => 29, line => 14}}}
                   %%     ]
                   }
               },
  assert_changes(Expected, Result).

-spec rename_macro_from_usage(config()) -> ok.
rename_macro_from_usage(Config) ->
  Uri = ?config(rename_usage1_uri, Config),
  Line = 15,
  Char = 7,
  NewName = <<"NEW_AWESOME_NAME">>,
  #{result := Result} = els_client:document_rename(Uri, Line, Char, NewName),
  Expected =  #{changes =>
                  #{ binary_to_atom(?config(rename_h_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 17, line => 0}
                               , start => #{character => 8, line => 0}}}
                       ]
                   , binary_to_atom(?config(rename_usage1_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 14, line => 15}
                               , start => #{character => 4, line => 15}}}
                       , #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 26, line => 15}
                               , start => #{character => 16, line => 15}}}
                       ]
                   , binary_to_atom(?config(rename_usage2_uri, Config), utf8) =>
                       [ #{ newText => NewName
                          , range =>
                              #{ 'end' => #{character => 27, line => 11}
                               , start => #{character => 17, line => 11}}}
                       ]
                   }
               },
  assert_changes(Expected, Result).

assert_changes(#{ changes := ExpectedChanges }, #{ changes := Changes }) ->
  ?assertEqual(maps:keys(ExpectedChanges), maps:keys(Changes)),
  Pairs = lists:zip(lists:sort(maps:to_list(Changes)),
                    lists:sort(maps:to_list(ExpectedChanges))),
  [ begin
      ?assertEqual(ExpectedKey, Key),
      ?assertEqual(Expected, Change)
    end
    || {{Key, Change}, {ExpectedKey, Expected}} <- Pairs
  ],
  ok.
