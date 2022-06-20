-module(code_navigation).

-behaviour(behaviour_a).
-wildattribute(a).
-export([ function_a/0, function_b/0, function_g/1, function_j/0, 'PascalCaseFunction'/1, function_mb/0 ]).

%% behaviour_a callbacks
-export([ callback_a/0 ]).
-export_type([ type_a/0 ]).
-import(code_navigation_extra, [ do/1 ]).
-include("transitive.hrl").
-include("code_navigation.hrl").
-include_lib("code_navigation/include/code_navigation.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(record_a, {field_a, field_b, 'Field C'}).

-define(MACRO_A, macro_a).
-define(MACRO_A(X), erlang:display(X)).

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
  ?MACRO_A(d).

function_e() ->
  ?assertEqual(2.0, 4/2).

-define(macro_A, macro_A).

function_f() ->
  ?macro_A.

function_g(X) ->
  F = fun function_b/0,
  G = {fun code_navigation_extra:do/1, X#included_record_a.field_b, X#'PascalCaseRecord'.'Field #1'},
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
  code_navigation_extra, 'Code.Navigation.Elixirish',
  function_m(code_navigation_extra).

%% [#386] go to definition of import by module
function_n() ->
  do(4).

%% atom highlighting and completion includes record fields
function_o() ->
  {field_a, incl}.

%% quoted atoms
-spec 'PascalCaseFunction'(T) -> 'Code.Navigation.Elixirish':'Type'(T).
'PascalCaseFunction'(R) ->
  _ = R#record_a.'Field C',
  F = fun 'Code.Navigation.Elixirish':do/1,
  F('Atom with whitespaces, "double quotes" and even some \'single quotes\'').

function_p(Foo) ->
  Bar = Foo,
  _Baz = Bar;
function_p(Foo) ->
  _Bar = Foo,
  _Baz = Foo.

%% [#1052] ?MODULE macro as record name
-record(?MODULE, {field_a, field_b}).

-spec function_q() -> {#?MODULE{field_a :: integer()}, any()}.
function_q() ->
  X = #?MODULE{},
  {X#?MODULE{field_a = 42}, X#?MODULE.field_a}.

-define(MACRO_B, macro_b).

macro_b(_X, _Y) ->
  ok.

function_mb() ->
  ?MACRO_B(m, b).

code_navigation() -> code_navigation.

code_navigation(X) -> X.

multiple_instances_same_file() -> {code_navigation, [simple_list], "abc"}.

code_navigation_extra(X, Y, Z) -> [code_navigation_extra, X, Y, Z].

multiple_instances_diff_file() -> code_navigation_extra.
