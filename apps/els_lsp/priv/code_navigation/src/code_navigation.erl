-module(code_navigation).

-behaviour(behaviour_a).
-wildattribute(a).
-export([ function_a/0, function_b/0, function_g/1, function_j/0 ]).

%% behaviour_a callbacks
-export([ callback_a/0 ]).

-import(code_navigation_extra, [ do/1 ]).

-include("code_navigation.hrl").
-include_lib("code_navigation/include/code_navigation.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(record_a, {field_a, field_b}).

-define(MACRO_A, macro_a).
-define(MACRO_WITH_ARGS(X), erlang:display(X)).

function_a() ->
  function_b(),
  #record_a{}.

function_b() ->
  ?MACRO_A.

callback_a() ->
  ok.

function_c() ->
  code_navigation_extra:do(test),
  A = #record_a{ field_a = a },
  _X = A#record_a.field_a, _Y = A#record_a.field_a,
  length([1, 2, 3]).

-type type_a() :: any().

function_d() ->
  ?MACRO_WITH_ARGS(d).

function_e() ->
  ?assertEqual(2.0, 4/2).

-define(macro_A, macro_A).

function_f() ->
  ?macro_A.

function_g(X) ->
  F = fun function_b/0,
  G = {fun code_navigation_extra:do/1, X#included_record_a.field_b},
  {?INCLUDED_MACRO_A, #included_record_a{included_field_a = a}, F, G}.

-spec function_h() -> type_a() | undefined_type_a() | file:fd().
function_h() ->
  function_i().

-ifdef(foo).
function_i() -> one.
-else.
function_i() -> two.
-endif.

%% @doc Such a wonderful function.
-spec function_j() -> pos_integer().
function_j() ->
  53.

%% [#283] Macro as function name crashes parser
?macro_A() -> ok.

%% [#333] Record field accessors assumed to be atoms
function_k() ->
  X#included_record_a.?MACRO_A,
  <<"foo:">>.

%% [#314] Add '_' to unused variable
function_l(X, Y) ->
    A = X,
    Y.

%% [#485] atoms referencing a module
function_m(code_navigation_types) ->
  code_navigation_extra,
  function_m(code_navigation_extra).

%% [#386] go to definition of import by module
function_n() ->
  do(4).

%% atom highlighting and completion includes record fields
function_o() ->
  {field_a, incl}.
