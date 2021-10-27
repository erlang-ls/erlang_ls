-module(completion_resolve_2).

-type mytype(T) :: [T].
%% Hello
-opaque myopaque(T) :: [T].
%% Is there anybody in there

-export_type([mytype/1,myopaque/1]).