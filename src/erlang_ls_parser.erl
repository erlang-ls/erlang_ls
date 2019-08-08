-module(erlang_ls_parser).

-export([ parse/1
        , parse_file/1
        ]).

%% TODO: Generate random filename
%% TODO: Ideally avoid writing to file at all (require epp changes)
-define(TMP_PATH, <<"/tmp/erlang_ls_tmp">>).

-spec parse(binary()) ->
   {ok, erlang_ls_tree:tree(), erlang_ls_tree:extra()} | {error, any()}.
parse(Text) ->
  %% epp_dodger only works with source files,
  %% so let's use a temporary file.
  ok = file:write_file(?TMP_PATH, Text),
  parse_file(?TMP_PATH).

-spec parse_file(binary()) ->
   {ok, erlang_ls_tree:tree(), erlang_ls_tree:extra()} | {error, any()}.
parse_file(Path) ->
  case file:open(Path, [read]) of
    {ok, IoDevice} ->
      %% Providing `{1, 1}` as the initial location ensures
      %% that the returned forms include column numbers, as well.
      %% The specs for the epp_dodger API are slightly incorrect.
      %% A bug has been reported (see https://bugs.erlang.org/browse/ERL-1005)
      %% Meanwhile, let's trick Dialyzer with an apply.
      {ok, Forms} = erlang:apply(epp_dodger, parse, [IoDevice, {1, 1}]),
      Tree = erl_syntax:form_list(Forms),
      ok = file:close(IoDevice),
      {ok, Extra} = parse_extra(Path),
      {ok, Tree, Extra};
    {error, Error} ->
      {error, Error}
  end.

-spec parse_extra(binary()) ->
   {ok, erlang_ls_tree:extra()} | {error, any()}.
parse_extra(Path) ->
  case file:open(Path, [read]) of
    {ok, IoDevice} ->
      {ok, Extra} = parse_extra(IoDevice, #{}, {1, 1}),
      ok = file:close(IoDevice),
      {ok, Extra};
    {error, Error} ->
      {error, Error}
  end.

-spec parse_extra(io:device(), erlang_ls_tree:extra(), erl_anno:location()) ->
   {ok, erlang_ls_tree:extra()} | {error, any()}.
parse_extra(IoDevice, Extra, StartLocation) ->
  case io:scan_erl_form(IoDevice, "", StartLocation) of
    {ok, Tokens, EndLocation} ->
      case erl_parse:parse_form(Tokens) of
        {ok, Form} ->
          parse_extra(IoDevice, extra(Form, Tokens, Extra), EndLocation);
        {error, _Error} ->
          parse_extra(IoDevice, Extra, EndLocation)
      end;
    {eof, _} ->
      {ok, Extra};
    {error, Error} ->
      {error, Error}
  end.

-spec extra( erl_parse:abstract_form()
           , [erl_scan:token()]
           , erlang_ls_tree:extra()) -> erlang_ls_tree:extra().
extra(Form, Tokens, Extra) ->
  Type = erl_syntax:type(Form),
  extra(Form, Tokens, Extra, Type).

-spec extra(erl_parse:abstract_form(), [erl_scan:token()], erlang_ls_tree:extra(), atom()) ->
   erlang_ls_tree:extra().
extra(Form, Tokens, Extra, attribute) ->
  case erl_syntax_lib:analyze_attribute(Form) of
    {export, Exports} ->
      %% TODO: Move to function
      OldLocations = maps:get(exports_locations, Extra, []),
      Locations = [L || {atom, L, F} <- Tokens, F =/= export],
      NewLocations = lists:append( OldLocations
                                 , lists:zip(Exports, Locations)
                                 ),
      maps:put(exports_locations, NewLocations, Extra);
    _ ->
      Extra
  end;
extra(_Form, _Tokens, Extra, _Type) ->
  Extra.

%% TODO: Add with_file
