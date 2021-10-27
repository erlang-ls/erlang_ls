-module(completion_resolve).

-export([ call_3/3
        ]).

-opaque myopaque() :: term().
%% This is my opaque

-type mytype() :: completion_resolve_type:myopaque().
%% This is my type

-export_type([myopaque/0]).

-spec call_3({mytype(), completion_resolve_type:myopaque()},
             completion_resolve_type_2:mytype(list()),
             completion_resolve_type_2:myopaque(list())) ->
  file:name_all().
call_3(_A, _B, _C) ->
  ok.

