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

  case {is_windows(), Host} of
    {true, <<>>} ->
      % Windows drive letter, have to strip the initial slash
      re:replace(
        Path, "^/([a-zA-Z]:)(.*)", "\\1\\2", [{return, binary}]
      );
    {true, _} ->
      <<"//", Host/binary, Path/binary>>;
    {false, <<>>} ->
      Path;
    {false, _} ->
      error(badarg)
  end.

-spec uri(path()) -> uri().
uri(Path) ->
  [Head | Tail] = filename:split(Path),
  {Host, Path1} = case {is_windows(), Head} of
                   {false, <<"/">>} ->
                     {<<>>, lists:join(<<"/">>, Tail)};
                   {true, X} when X =:= <<"//">> orelse X =:= <<"\\\\">> ->
                     [H | T] = Tail,
                     {H, lists:join(<<"/">>, T)};
                   {true, _} ->
                     {<<>>, lists:join(<<"/">>, [Head | Tail])}
                 end,

  els_utils:to_binary(
    uri_string:recompose(#{
      scheme => <<"file">>, host => Host, path => [<<"/">>, Path1]
    })
  ).

-spec is_windows() -> boolean().
is_windows() ->
  {OS, _} = os:type(),
  OS =:= win32.
