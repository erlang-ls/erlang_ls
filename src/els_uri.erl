%%==============================================================================
%% Library to parse RFC-3986 URIs
%%==============================================================================
%% For details, see: http://tools.ietf.org/html/rfc3986
%%==============================================================================
-module(els_uri).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ module/1
        , path/1
        , uri/1
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type path() :: binary().

-export_type([ path/0 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

-spec module(uri()) -> atom().
module(Uri) ->
  binary_to_atom(filename:basename(path(Uri), <<".erl">>), utf8).

-spec path(uri()) -> path().
path(Uri) ->
  Uri1 = http_uri:decode(Uri),
  #{ host := Host
   , path := Path
   , scheme := <<"file">>
  } = uri_string:parse(Uri1),

  case {is_windows(), is_local(Host)} of
    {_, true} ->
      Path;
    {true, false} ->
      <<"//", Host/binary, Path/binary>>;
    {false, false} ->
      error(badarg)
  end.

-spec uri(path()) -> uri().
uri(Path) ->
  Path1 = case is_windows() of
           true -> els_utils:to_binary(string:replace(Path, "\\", "/"));
           false -> Path
         end,
  <<"file://", Path1/binary>>.

-spec is_windows() -> boolean().
is_windows() ->
  {OS, _} = os:type(),
  OS =:= win32.

-spec is_local(binary()) -> boolean().
is_local(Host) ->
  Host =:= <<>> orelse Host =:= <<"localhost">>.
