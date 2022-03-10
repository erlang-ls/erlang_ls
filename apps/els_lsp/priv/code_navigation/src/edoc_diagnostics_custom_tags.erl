-module(edoc_diagnostics_custom_tags).

-export([ a/0 ]).

%% @edoc
%% `edoc' is a custom alias for `doc'
a() ->
    ok.

%% @docc
%% `docc' is not an existing or custom tag
b() ->
    ok.

%% @generated
%% The `generated' tag is used for generated code
c() ->
    ok.
