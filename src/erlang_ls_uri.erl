%%==============================================================================
%% Library to parse RFC-3986 URIs
%%==============================================================================
%% For details, see: http://tools.ietf.org/html/rfc3986
%%==============================================================================
-module(erlang_ls_uri).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ basename/1
        , filename/1
        , module/1
        , path/1
        , uri/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

-spec basename(uri()) -> binary().
basename(Uri) ->
  filename:basename(path(Uri)).

-spec filename(atom()) -> binary().
filename(Module) ->
  list_to_binary(atom_to_list(Module) ++ ".erl").

-spec module(uri()) -> atom().
module(Uri) ->
  binary_to_atom(filename:basename(basename(Uri), <<".erl">>), utf8).

-spec path(uri()) -> uri_path().
path(<<"file://", Path/binary>>) ->
  Path.

-spec uri(uri_path()) -> uri().
uri(Path) ->
  <<"file://", Path/binary>>.
