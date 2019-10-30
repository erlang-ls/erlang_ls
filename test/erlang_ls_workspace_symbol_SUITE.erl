-module(erlang_ls_workspace_symbol_SUITE).

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


-include("erlang_ls.hrl").

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
-spec query_multiple(config()) -> ok.
query_multiple(Config) ->
  Query = <<"code_navigation">>,
  Uri = ?config(code_navigation_uri, Config),
  ExtraUri = ?config(code_navigation_extra_uri, Config),
  #{result := Result} = erlang_ls_client:workspace_symbol(Query),
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
             ],
  ?assertEqual(lists:sort(Expected), lists:sort(Result)),
  ok.

-spec query_single(config()) -> ok.
query_single(Config) ->
  Query = <<"extra">>,
  ExtraUri = ?config(code_navigation_extra_uri, Config),
  #{result := Result} = erlang_ls_client:workspace_symbol(Query),
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
  #{result := Result} = erlang_ls_client:workspace_symbol(<<"invalid_query">>),
  ?assertEqual([], Result),
  ok.
