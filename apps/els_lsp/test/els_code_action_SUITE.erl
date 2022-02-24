-module(els_code_action_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ add_underscore_to_unused_var/1
        , export_unused_function/1
        , suggest_variable/1
        , fix_module_name/1
        , remove_unused_macro/1
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
%% Const
%%==============================================================================
-define(COMMENTS_LINES, 2).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec add_underscore_to_unused_var(config()) -> ok.
add_underscore_to_unused_var(Config) ->
  Uri = ?config(code_action_uri, Config),
  Range = els_protocol:range(#{from => {?COMMENTS_LINES + 6, 3}
                              , to => {?COMMENTS_LINES + 6, 4}}),
  Diag = #{ message  => <<"variable 'A' is unused">>
          , range    => Range
          , severity => 2
          , source   => <<"Compiler">>
          },
  #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
  Expected =
    [ #{ edit => #{changes =>
                     #{ binary_to_atom(Uri, utf8) =>
                          [ #{ range => Range
                             , newText => <<"_A">>
                             }]
                      }}
       , kind => <<"quickfix">>
       , title => <<"Add '_' to 'A'">>
       }
    ],
  ?assertEqual(Expected, Result),
  ok.

-spec export_unused_function(config()) -> ok.
export_unused_function(Config) ->
  Uri = ?config(code_action_uri, Config),
  Range = els_protocol:range(#{from => {?COMMENTS_LINES + 12, 1}
                              , to => {?COMMENTS_LINES + 12, 10}}),
  Diag = #{ message  => <<"function function_c/0 is unused">>
          , range    => Range
          , severity => 2
          , source   => <<"Compiler">>
          },
  #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
  Expected =
    [ #{ edit => #{changes =>
                     #{ binary_to_atom(Uri, utf8) =>
                          [ #{ range =>
                                 #{'end' => #{ character => 0
                                              , line => ?COMMENTS_LINES + 3},
                                   start => #{character => 0
                                              , line => ?COMMENTS_LINES + 3}}
                             , newText => <<"-export([function_c/0]).\n">>
                             }
                          ]}
                  }
       , kind => <<"quickfix">>
       , title => <<"Export function_c/0">>
       }
    ],
  ?assertEqual(Expected, Result),
  ok.

-spec suggest_variable(config()) -> ok.
suggest_variable(Config) ->
  Uri = ?config(code_action_uri, Config),
  Range = els_protocol:range(#{from => {?COMMENTS_LINES + 15, 9}
                              , to => {?COMMENTS_LINES + 15, 13}}),
  Diag = #{ message  => <<"variable 'Barf' is unbound">>
          , range    => Range
          , severity => 3
          , source   => <<"Compiler">>
          },
  #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
  Expected =
    [ #{ edit => #{changes =>
                     #{ binary_to_atom(Uri, utf8) =>
                          [#{ range => Range
                            , newText => <<"Bar">>
                            }]
                      }}
       , kind => <<"quickfix">>
       , title => <<"Did you mean 'Bar'?">>
       }
    ],
  ?assertEqual(Expected, Result),
  ok.

-spec fix_module_name(config()) -> ok.
fix_module_name(Config) ->
  Uri = ?config(code_action_uri, Config),
  Range = els_protocol:range(#{from => {?COMMENTS_LINES + 1, 9}
                              , to => {?COMMENTS_LINES + 1, 25}}),
  Diag = #{ message  => <<"Module name 'code_action_oops' does not "
                          "match file name 'code_action'">>
          , range    => Range
          , severity => 3
          , source   => <<"Compiler">>
          },
  #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
  Expected =
    [ #{ edit => #{changes =>
                     #{ binary_to_atom(Uri, utf8) =>
                          [#{ range => Range
                            , newText => <<"code_action">>
                            }]
                      }}
       , kind => <<"quickfix">>
       , title => <<"Change to -module(code_action).">>
       }
    ],
  ?assertEqual(Expected, Result),
  ok.

-spec remove_unused_macro(config()) -> ok.
remove_unused_macro(Config) ->
  Uri = ?config(code_action_uri, Config),
  Range = els_protocol:range(#{from => {?COMMENTS_LINES + 17, 9}
                              , to => {?COMMENTS_LINES + 17, 15}}),
  LineRange = els_range:line(#{from => {?COMMENTS_LINES + 17, 9}
                              , to => {?COMMENTS_LINES + 17, 15}}),
  Diag = #{ message  => <<"Unused macro: TIMEOUT">>
          , range    => Range
          , severity => 2
          , source   => <<"UnusedMacros">>
          },
  #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
  Expected =
    [ #{ edit => #{changes =>
                     #{ binary_to_atom(Uri, utf8) =>
                          [#{ range => els_protocol:range(LineRange)
                            , newText => <<"">>
                            }]
                      }}
       , kind => <<"quickfix">>
       , title => <<"Remove unused macro TIMEOUT.">>
       }
    ],
  ?assertEqual(Expected, Result),
  ok.
