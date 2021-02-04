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
-include("els_core.hrl").

-spec module(uri()) -> atom().
module(Uri) ->
  binary_to_atom(filename:basename(path(Uri), <<".erl">>), utf8).

-spec path(uri()) -> path().
path(Uri) ->
  #{ host := Host
   , path := Path
   , scheme := <<"file">>
   } = uri_string:normalize(Uri, [return_map]),

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
                     {<<>>, uri_join(Tail)};
                   {true, X} when X =:= <<"//">> orelse X =:= <<"\\\\">> ->
                     [H | T] = Tail,
                     {H, uri_join(T)};
                   {true, _} ->
                     % Strip the trailing slash from the first component
                     H1 = string:slice(Head, 0, 2),
                     {<<>>, uri_join([H1|Tail])}
                 end,

  els_utils:to_binary(
    uri_string:recompose(#{
      scheme => <<"file">>,
      host => Host,
      path => [$/, Path1]
    })
  ).

-spec uri_join([path()]) -> iolist().
uri_join(List) ->
  lists:join(<<"/">>, List).

-spec is_windows() -> boolean().
is_windows() ->
  {OS, _} = os:type(),
  OS =:= win32.
