-module(els_workspace_symbol_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ query_multiple/1
        , query_single/1
        , query_none/1
        ]).


-include("els_lsp.hrl").

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
-spec query_multiple(config()) -> ok.
query_multiple(Config) ->
  Query = <<"code_navigation">>,
  Uri = ?config(code_navigation_uri, Config),
  ExtraUri = ?config(code_navigation_extra_uri, Config),
  TypesUri = ?config(code_navigation_types_uri, Config),
  UndefUri = ?config(code_navigation_undefined_uri, Config),
  BrokenUri = ?config(code_navigation_broken_uri, Config),
  #{result := Result} = els_client:workspace_symbol(Query),
  Expected = [ #{ kind => ?SYMBOLKIND_MODULE
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => 0, line => 0}
                          , start => #{character => 0, line => 0}
                          }
                     , uri  => Uri
                     }
                , name => <<"code_navigation">>
                }
             , #{ kind => ?SYMBOLKIND_MODULE
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => 0, line => 0}
                          , start => #{character => 0, line => 0}
                          }
                     , uri  => ExtraUri
                     }
                , name => <<"code_navigation_extra">>
                }
             , #{ kind => ?SYMBOLKIND_MODULE
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => 0, line => 0}
                          , start => #{character => 0, line => 0}
                          }
                     , uri  => TypesUri
                     }
                , name => <<"code_navigation_types">>
                }
             , #{ kind => ?SYMBOLKIND_MODULE
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => 0, line => 0}
                          , start => #{character => 0, line => 0}
                          }
                     , uri  => UndefUri
                     }
                , name => <<"code_navigation_undefined">>
                }
             , #{ kind => ?SYMBOLKIND_MODULE
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => 0, line => 0}
                          , start => #{character => 0, line => 0}
                          }
                     , uri  => BrokenUri
                     }
                , name => <<"code_navigation_broken">>
                }
             ],
  ?assertEqual(lists:sort(Expected), lists:sort(Result)),
  ok.

-spec query_single(config()) -> ok.
query_single(Config) ->
  Query = <<"extra">>,
  ExtraUri = ?config(code_navigation_extra_uri, Config),
  #{result := Result} = els_client:workspace_symbol(Query),
  Expected = [ #{ kind => ?SYMBOLKIND_MODULE
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => 0, line => 0}
                          , start => #{character => 0, line => 0}
                          }
                     , uri  => ExtraUri
                     }
                , name => <<"code_navigation_extra">>
                }
             ],
  ?assertEqual(lists:sort(Expected), lists:sort(Result)),
  ok.

-spec query_none(config()) -> ok.
query_none(_Config) ->
  #{result := Result} = els_client:workspace_symbol(<<"invalid_query">>),
  ?assertEqual([], Result),
  ok.
