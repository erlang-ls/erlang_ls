-module(els_code_action_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({document_codeaction, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}
   , <<"range">>        := RangeLSP
   , <<"context">>      := Context } = Params,
  Result = code_actions(Uri, RangeLSP, Context),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================


%% @doc Result: `(Command | CodeAction)[] | null'
-spec code_actions(uri(), range(), code_action_context()) -> [map()].
code_actions(Uri, _Range, Context) ->
  #{ <<"diagnostics">> := Diagnostics } = Context,
  Actions0 = [ make_code_action(Uri, D) || D <- Diagnostics],
  Actions = lists:flatten(Actions0),
  Actions.

%% @doc Note: if the start and end line of the range are the same, the line
%% is simply added.
-spec replace_lines_action(uri(), binary(), binary(), binary(), range())
                          -> map().
replace_lines_action(Uri, Title, Kind, Lines, Range) ->
  #{ <<"start">> := #{ <<"character">> := _StartCol
                     , <<"line">>      := StartLine }
   , <<"end">>   := #{ <<"character">> := _EndCol
                     , <<"line">>      := EndLine }
   } = Range,
  #{ title => Title
   , kind => Kind
   , command =>
       els_command:make_command( Title
                               , <<"replace-lines">>
                               , [#{ uri   => Uri
                                   , lines => Lines
                                   , from  => StartLine
                                   , to    => EndLine }])
   }.

-spec make_code_action(uri(), diagnostic()) -> [map()].
make_code_action(Uri, #{ <<"message">> := Message
                       , <<"range">>   := Range } = _Diagnostic) ->
  unused_variable_action(Uri, Range, Message).

%%------------------------------------------------------------------------------

-spec unused_variable_action(uri(), range(), binary()) -> [map()].
unused_variable_action(Uri, Range, Message) ->
  %% Processing messages like "variable 'Foo' is unused"
  case re:run(Message, "variable '(.*)' is unused"
             , [{capture, all_but_first, binary}]) of
      {match, [UnusedVariable]} ->
          make_unused_variable_action(Uri, Range, UnusedVariable);
      _ -> []
  end.

-spec make_unused_variable_action(uri(), range(), binary()) -> [map()].
make_unused_variable_action(Uri, Range, UnusedVariable) ->
  #{ <<"start">> := #{ <<"character">> := _StartCol
                     , <<"line">>      := StartLine }
   , <<"end">>   := _End
   } = Range,
  %% processing messages like "variable 'Foo' is unused"
  {ok, #{text := Bin}} = els_utils:lookup_document(Uri),
  Line = els_utils:to_list(els_text:line(Bin, StartLine)),

  {ok, Tokens, _} = erl_scan:string(Line, 1, [return, text]),
  UnusedString = els_utils:to_list(UnusedVariable),
  Replace =
        fun(Tok) ->
            case Tok of
                {var, [{text, UnusedString}, _], _} -> "_" ++ UnusedString;
                {var, [{text, VarName}, _], _} -> VarName;
                {_,   [{text, Text   }, _], _} -> Text;
                {_,   [{text, Text   }, _]}    -> Text
            end
  end,
  UpdatedLine = lists:flatten(lists:map(Replace, Tokens)) ++ "\n",
    [ replace_lines_action( Uri
                      , <<"Add '_' to '", UnusedVariable/binary, "'">>
                      , ?CODE_ACTION_KIND_QUICKFIX
                      , els_utils:to_binary(UpdatedLine)
                      , Range)].

%%------------------------------------------------------------------------------
