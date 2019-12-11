-module(els_code_action_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

%% /**
%%  * The server provides code actions. The `CodeActionOptions` return type is
%%  * only valid if the client signals code action literal support via the
%%  * property `textDocument.codeAction.codeActionLiteralSupport`.
%%  */
%% codeActionProvider?: boolean | CodeActionOptions;

-spec options() -> boolean() | map().
options() ->
  Capabilities = els_config:get(capabilities),
  lager:info("code_actions: [Capabilities=~p]", [Capabilities]),
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({document_codeaction, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}
   , <<"range">>   := RangeLSP
   , <<"context">> := Context } = Params,
  Result = code_actions(Uri, RangeLSP, Context),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================


%% result: (Command | CodeAction)[] | null where CodeAction is defined
%% as follows:
-spec code_actions(uri(), range(), code_action_context()) -> [map()].
code_actions(Uri, Range, Context) ->
  lager:info("code_actions: [Context=~p]", [Context]),
  #{ <<"start">> := #{ <<"character">> := _StartCol
                     , <<"line">>      := StartLine }
   , <<"end">> := _End
   } = Range,
  #{ <<"diagnostics">> := Diagnostics } = Context,
  Actions0 = [ make_code_action(Uri, C) || C <- Diagnostics],

  Actions = case application:get_env(erlang_ls, test_code_action) of
    {ok, true} -> [ add_lines_action( Uri
                                    , <<"Add TODO comment">>
                                    , <<"%% TODO: something\n">>
                                    , StartLine)]
                   ++ Actions0;
    _ -> Actions0
  end,
  lager:info("code_actions: [Actions=~p]", [Actions]),
  Actions.


-spec add_lines_action(uri(), binary(), binary(), number()) -> map().
add_lines_action(Uri, Title, Lines, Before) ->
   #{ title => Title
    , command =>
          els_utils:make_command( Title
                                , <<"add-lines">>
                                , [#{ uri => Uri
                                    , lines => Lines
                                    , before => Before }])
    }.

-spec make_code_action(uri(), diagnostic()) -> [code_action()].
make_code_action(Uri, #{ <<"message">> := Message
                       , <<"range">> := Range } = _Diagnostic) ->
    lager:info("make_code_action: [Message=~p]", [Message]),
    %% processing messages of the type "type type_name() undefined".
    %% Good resource:
%% https://arifishaq.wordpress.com/2013/11/13/playing-with-regular-expressions/
    %% TODO: this is a hot path, precompile the re's.
    case re:run(Message, "type (.*) undefined"
               , [{capture, all_but_first, binary}]) of
        {match, [UndefinedType]} ->
            undefined_type_action(Uri, Range, UndefinedType);
        _ -> []
    end.


-spec undefined_type_action(uri(), range(), binary()) -> [code_action()].
undefined_type_action(Uri, _Range, UndefinedType) ->
    lager:info("undefined_type_action: [UndefinedType=~p]", [UndefinedType]),
    MFA = parse_mfa(UndefinedType),
    lager:info("undefined_type_action: [MFA=~p]", [MFA]),

    Poi = els_poi:new(#{from => {0, 0}, to => {0, 0}}, type_application, MFA),
    Res = els_code_navigation:goto_definition(Uri, Poi),
    lager:info("undefined_type_action: [Res=~p]", [Res]),
    [].

-spec parse_mfa(binary()) -> any().
parse_mfa(NameToParse) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(NameToParse) ++ "."),
    {ok, [{call, _, Name, Args}]} = erl_parse:parse_exprs(Tokens),
    lager:info("parse_mfa: [{Name,Args}=~p]", [{Name, Args}]),
    MFA = case Name of
            {atom, 1, Name0} -> { Name0
                                , length(Args)};
            {remote, 1, M, N} -> { erl_syntax:atom_value(M)
                                 , erl_syntax:atom_value(N)
                                 , length(Args)}
          end,
    MFA.
