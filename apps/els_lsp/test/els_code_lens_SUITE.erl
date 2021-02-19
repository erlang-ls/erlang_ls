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
        , show_exported_test/1
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
init_per_testcase(server_info, Config0) ->
  Config1 =
    setup_lenses(
      [els_code_lens_server_info],
      [ els_code_lens_show_exported
      , els_code_lens_suggest_spec], Config0
    ),
  els_test_utils:init_per_testcase(server_info, Config1);
init_per_testcase(ct_run_test, Config0) ->
  Config1 =
    setup_lenses(
      [els_code_lens_ct_run_test],
      [ els_code_lens_show_exported
      , els_code_lens_suggest_spec], Config0
    ),
  els_test_utils:init_per_testcase(ct_run_test, Config1);
init_per_testcase(show_exported_test, Config0) ->
  Config1 =
    setup_lenses(
      [els_code_lens_show_exported],
      [els_code_lens_suggest_spec], Config0
    ),
  els_test_utils:init_per_testcase(show_exported_test, Config1);
init_per_testcase(TestCase, Config) ->
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  cleanup_lenses(Config),
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% helpers
%%==============================================================================
-spec setup_lenses(Enabled :: [module()], Disabled :: [module()], Config :: config()) -> config().
setup_lenses(Enabled, Disabled, Config) ->
  [ meck:new(Mock, [passthrough, no_link]) || Mock <- Enabled ++ Disabled ],
  [ meck:expect(Mock, is_default, 0, true) || Mock <- Enabled ],
  [ meck:expect(Mock, is_default, 0, false) || Mock <- Disabled ],
  [ {lens_mocks, Enabled ++ Disabled} | Config ].


-spec cleanup_lenses(config()) -> ok.
cleanup_lenses(Config) ->
  Mocks = proplists:get_value(lens_mocks, Config, []),
  [meck:unload(Mock) || Mock <- Mocks],
  ok.

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
  ?assertEqual(15, length(Commands)),
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


-spec show_exported_test(config()) -> ok.
show_exported_test(Config) ->
  Uri = ?config(code_navigation_extra_uri, Config),
  PrefixedCommand = els_command:with_prefix(<<"show-exported">>),
  #{result := Result} = els_client:document_codelens(Uri),
  %% match the first function ignoroing the position
  ?assertMatch(
    [#{ command :=
          #{ arguments := [],command := PrefixedCommand
          , title := <<"exported (2 references)">>}
      , data := []
      , range := _} | _],
    Result),
  ok.
