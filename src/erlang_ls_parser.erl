-module(erlang_ls_parser).

-export([ parse/1
        , parse_file/1
        , parse_exports/1
        ]).

%% TODO: Generate random filename
%% TODO: Ideally avoid writing to file at all (require epp changes)
-define(TMP_PATH, <<"/tmp/erlang_ls_tmp">>).

-spec parse(binary()) -> {ok, erlang_ls_tree:tree()}.
parse(Text) ->
  %% epp_dodger only works with source files,
  %% so let's use a temporary file.
  ok = file:write_file(?TMP_PATH, Text),
  parse_file(?TMP_PATH).

-spec parse_file(binary()) ->
   {ok, erlang_ls_tree:tree()} | {error, any()}.
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
      {ok, Tree};
    {error, Error} ->
      {error, Error}
  end.

-spec parse_exports(binary()) ->
   {ok, [[erlang_ls_poi:pos()]]} | {error, any()}.
parse_exports(Path) ->
  case file:open(Path, [read]) of
    {ok, IoDevice} ->
      Res = parse_exports(IoDevice, [], {1, 1}),
      ok = file:close(IoDevice),
      {ok, Res};
    {error, Error} ->
      {error, Error}
  end.

-spec parse_exports(io:device(), [any()], erlang_ls_poi:pos()) ->
   {ok, [[erlang_ls_poi:pos()]]} | {error, any()}.
parse_exports(IoDevice, Acc, StartLocation) ->
  case io:scan_erl_form(IoDevice, "", StartLocation) of
    {ok, Tokens, EndLocation} ->
      case erl_parse:parse_form(Tokens) of
        {ok, Form} ->
          case erl_syntax:type(Form) of
            attribute ->
              case erl_syntax_lib:analyze_attribute(Form) of
                {export, Exports} ->
                  Locations = [L || {atom, L, F} <- Tokens, F =/= export],
                  parse_exports(IoDevice, [lists:zip(Exports, Locations)|Acc], EndLocation);
                _ ->
                  parse_exports(IoDevice, Acc, EndLocation)
              end;
            _ ->
              parse_exports(IoDevice, Acc, EndLocation)
          end;
        {error, _Error} ->
          parse_exports(IoDevice, Acc, EndLocation)
      end;
    {eof, _} ->
      lists:reverse(Acc);
    {error, Error} ->
      {error, Error}
  end.
