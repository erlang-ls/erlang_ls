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
-export([ query_modules/1
        , query_module/1
        , query_invalid/1
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
-spec query_modules(config()) -> ok.
query_modules(Config) ->
  #{result := Result} = erlang_ls_client:workspace_symbol(<<"m">>),
  Expected = [ #{ kind => Kind
                , location =>
                    #{ range =>
                         #{ 'end' => #{character => ToC, line => ToL}
                          , start => #{character => FromC, line => FromL}
                          }
                     , uri  => Uri
                     }
                , name => Name
                } || {Kind, Uri, Name, {FromL, FromC}, {ToL, ToC}}
                       <- lists:append([all_modules(Config)])],
  ?assertEqual(length(Expected), length(Result)),
  Pairs = lists:zip(lists:sort(Expected), lists:sort(Result)),
  [?assertEqual(E, S) || {E, S} <- Pairs],
  ok.

-spec query_module(config()) -> ok.
query_module(Config) ->
  Module = <<"m-code_navigation">>,
  Uri = ?config(code_navigation_uri, Config),
  #{result := Result} = erlang_ls_client:workspace_symbol(Module),
  Expected = [#{ kind => ?SYMBOLKIND_MODULE
               , location =>
                   #{ range =>
                        #{ 'end' => #{character => 0, line => 0}
                         , start => #{character => 0, line => 0}
                         }
                    , uri  => Uri
                    }
               , name => <<"code_navigation">>
               }],
  ?assertEqual(Expected, Result),
  ok.

-spec query_invalid(config()) -> ok.
query_invalid(_Config) ->
  #{result := Result} = erlang_ls_client:workspace_symbol(<<"invalid">>),
  ?assertEqual(null, Result),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
all_modules(Config) ->
  [ {?SYMBOLKIND_MODULE, ?config(behaviour_uri, Config),             <<"behaviour_a">>,           {0, 0}, {0, 0}}
  , {?SYMBOLKIND_MODULE, ?config(code_navigation_extra_uri, Config), <<"code_navigation_extra">>, {0, 0}, {0, 0}}
  , {?SYMBOLKIND_MODULE, ?config(code_navigation_uri, Config),       <<"code_navigation">>,       {0, 0}, {0, 0}}
  ].
