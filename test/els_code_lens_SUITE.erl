-module(els_code_lens_SUITE).

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
-export([ default_lenses/1
        , server_info/1
        , ct_run_test/1
        , show_behaviour_usages/1
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
init_per_testcase(server_info, Config) ->
  meck:new(els_code_lens_server_info, [passthrough, no_link]),
  meck:expect(els_code_lens_server_info, is_default, 0, true),
  %% Let's disable the suggest_spec lens to avoid noise
  meck:new(els_code_lens_suggest_spec, [passthrough, no_link]),
  meck:expect(els_code_lens_suggest_spec, is_default, 0, false),
  els_test_utils:init_per_testcase(server_info, Config);
init_per_testcase(ct_run_test, Config) ->
  meck:new(els_code_lens_ct_run_test, [passthrough, no_link]),
  meck:expect(els_code_lens_ct_run_test, is_default, 0, true),
  els_test_utils:init_per_testcase(server_info, Config);
init_per_testcase(TestCase, Config) ->
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(server_info, Config) ->
  els_test_utils:end_per_testcase(server_info, Config),
  meck:unload(els_code_lens_server_info),
  meck:unload(els_code_lens_suggest_spec),
  ok;
end_per_testcase(ct_run_test, Config) ->
  els_test_utils:end_per_testcase(ct_run_test, Config),
  meck:unload(els_code_lens_ct_run_test),
  ok;
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec default_lenses(config()) -> ok.
default_lenses(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Result} = els_client:document_codelens(Uri),
  Commands = [els_command:without_prefix(Command) ||
               #{command := #{command := Command }} <- Result],
  ?assertEqual([<<"suggest-spec">>], lists:usort(Commands)),
  ?assertEqual(14, length(Commands)),
  ok.

-spec server_info(config()) -> ok.
server_info(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Result} = els_client:document_codelens(Uri),
  PrefixedCommand = els_command:with_prefix(<<"server-info">>),
  Title = <<"Erlang LS (in code_navigation) info">>,
  Expected =
    [ #{ command => #{ arguments => []
                     , command   => PrefixedCommand
                     , title     => Title
                     }
       , data => []
       , range =>
           #{'end' => #{character => 0, line => 1},
             start => #{character => 0, line => 0}}
       }
    ],
  ?assertEqual(Expected, Result),
  ok.

-spec ct_run_test(config()) -> ok.
ct_run_test(Config) ->
  Uri = ?config(sample_SUITE_uri, Config),
  PrefixedCommand = els_command:with_prefix(<<"ct-run-test">>),
  #{result := Result} = els_client:document_codelens(Uri),
  Expected = [ #{ command => #{ arguments => [ #{ arity => 1
                                                , function => <<"one">>
                                                , line => 58
                                                , module => <<"sample_SUITE">>
                                                , uri => Uri
                                                }]
                              , command => PrefixedCommand
                              , title => <<"Run test">>
                              }
                , data => []
                , range => #{ 'end' => #{ character => 3
                                        , line => 57
                                        }
                            , start => #{ character => 0
                                        , line => 57
                                        }}}],
  ?assertEqual(Expected, Result),
  ok.

-spec show_behaviour_usages(config()) -> ok.
show_behaviour_usages(Config) ->
  Uri = ?config(behaviour_a_uri, Config),
  PrefixedCommand = els_command:with_prefix(<<"show-behaviour-usages">>),
  #{result := Result} = els_client:document_codelens(Uri),
  Expected = [#{ command =>
                   #{ arguments => []
                    , command => PrefixedCommand
                    , title => <<"Behaviour used in 1 place(s)">>
                    }
               , data => []
               , range =>
                   #{ 'end' => #{character => 19, line => 0}
                    , start => #{character => 8, line => 0}
                    }}],
  ?assertEqual(Expected, Result),
  ok.
