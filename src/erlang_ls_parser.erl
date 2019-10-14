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
    {error, ErrorInfo, EndLocation} ->
      {error, {ErrorInfo, EndLocation}}
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
      %% TODO: Use maps:update_with
      OldLocations = maps:get(exports_locations, Extra, []),
      %% Hackity-hack. The first atom is the attribute name.
      %% We should find a nicer way to parse the export list.
      [_|Locations] = [L || {atom, L, _F} <- Tokens],
      NewLocations = lists:append( OldLocations
                                 , lists:zip(Exports, Locations)
                                 ),
      maps:put(exports_locations, NewLocations, Extra);
    {import, {_M, Imports}} ->
      %% Hackity-hack. The first two atoms are the attribute name and
      %% the import module. We should find a nicer way to parse the
      %% import list.
      OldLocations = maps:get(import_locations, Extra, []),
      [_, _|Locations] = [L || {atom, L, _F} <- Tokens],
      NewLocations = lists:append( OldLocations
                                 , lists:zip(Imports, Locations)
                                 ),
      maps:put(import_locations, NewLocations, Extra);
    {spec, {spec, {{F, A}, [FT]}}} ->
      OldLocations = maps:get(spec_locations, Extra, []),
      NewLocations = [{{F, A}, spec_locations(FT)}|OldLocations],
      maps:put(spec_locations, NewLocations, Extra);
    _ ->
      Extra
  end;
extra(_Form, _Tokens, Extra, _Type) ->
  Extra.

%% TODO: Refine any() type
-spec spec_locations(any()) -> [{atom(), erl_anno:location()}].
spec_locations(FT) ->
  case erl_syntax:type(FT) of
    function_type ->
      FTR = erl_syntax:function_type_return(FT),
      FTA = erl_syntax:function_type_arguments(FT),
      do_spec_locations([FTR | FTA], []);
    constrained_function_type ->
      %% TODO
      []
  end.

-spec do_spec_locations([any()], [{atom(), erl_anno:location()}]) ->
   [{atom(), erl_anno:location()}].
do_spec_locations(any, Acc) ->
  Acc;
do_spec_locations([], Acc) ->
  Acc;
do_spec_locations([{type, StartLocation, Type, any}|T], Acc) ->
  do_spec_locations(T, [{Type, StartLocation}|Acc]);
do_spec_locations([{type, StartLocation, Type, Args}|T], Acc) ->
  do_spec_locations(T ++ Args, [{Type, StartLocation}|Acc]);
do_spec_locations([{user_type, StartLocation, Type, any}|T], Acc) ->
  do_spec_locations(T, [{Type, StartLocation}|Acc]);
do_spec_locations([{user_type, StartLocation, Type, Args}|T], Acc) ->
  do_spec_locations(T ++ Args, [{Type, StartLocation}|Acc]);
do_spec_locations([_Else|T], Acc) ->
  do_spec_locations(T, Acc).

%% TODO: Support type
%% TODO: Support remote_type
%% TODO: Support tuple
%% TODO: Support union
%% TODO: Add arity to type_definition
%% TODO: Check why proc_lib fails to parse
