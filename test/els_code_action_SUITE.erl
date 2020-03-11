-module(els_code_action_SUITE).

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
-export([ add_underscore_to_unused_var/1
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
-spec add_underscore_to_unused_var(config()) -> ok.
add_underscore_to_unused_var(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  Diag = #{ message  => <<"variable 'A' is unused">>
          , range    => #{ 'end' => #{character => 0, line => 80}
                         , start => #{character => 0, line => 79}
                         }
          , severity => 2
          , source   => <<"Compiler">>
          },
  Range = els_protocol:range(#{from => {80, 1}, to => {81, 1}}),
  PrefixedCommand
    = els_execute_command_provider:add_server_prefix(<<"replace-lines">>),
  #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
  Expected =
    [ #{ command => #{ arguments => [ #{ from  => 79
                                       , lines => <<"    _A = X,\n">>
                                       , to    => 80
                                       , uri   => Uri
                                       }
                                    ]
                     , command   => PrefixedCommand
                     , title     => <<"Add '_' to 'A'">>
                     }
       , kind    => <<"quickfix">>
       , title   => <<"Add '_' to 'A'">>
       }
    ],
  ?assertEqual(Expected, Result),
  ok.


%%==============================================================================
%% Internal functions
%%==============================================================================
