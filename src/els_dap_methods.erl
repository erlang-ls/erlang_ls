-module(els_dap_methods).

-include("erlang_ls.hrl").

-export([ dispatch/4
        ]).

-type method_name()  :: binary().
-type state()        :: map().
-type params()       :: map().
-type result()       :: {response, params() | null, state()}
                      | {error, params(), state()}
                      | {noresponse, state()}
                      | {notification, binary(), params(), state()}.
-type request_type() :: notification | request.

%%==============================================================================
%% @doc Dispatch the handling of the method to els_method
%%==============================================================================
-spec dispatch(method_name(), params(), request_type(), state()) -> result().
dispatch(Command, Args, Type, State) ->
  lager:debug("Dispatching request [command=~p] [args=~p]", [Command, Args]),
  try do_dispatch(Command, Args, State)
  catch
    error:undef ->
      not_implemented_method(Command, Type, State);
    Type:Reason:Stack ->
      lager:error( "Unexpected error [type=~p] [error=~p] [stack=~p]"
                 , [Type, Reason, Stack]),
      Error = #{ code    => ?ERR_UNKNOWN_ERROR_CODE
               , message => <<"Unexpected error while ", Command/binary>>
               },
      {error_response, Error, State}
  end.

-spec do_dispatch(atom(), params(), state()) -> result().
do_dispatch(Command, Args, #{status := initialized} = State) ->
  Request = {Command, Args},
  Result = els_provider:handle_request(els_dap_general_provider, Request),
  {response, Result, State};
do_dispatch(<<"initialize">>, Args, State) ->
  Request = {<<"initialize">>, Args},
  Result = els_provider:handle_request(els_dap_general_provider, Request),
  {response, Result, State#{status => initialized}};
do_dispatch(_Command, _Args, State) ->
  Message = <<"The server is not fully initialized yet, please wait.">>,
  Result  = #{ code    => ?ERR_SERVER_NOT_INITIALIZED
             , message => Message
             },
  {error, Result, State}.

-spec not_implemented_method(method_name(), atom(), state()) -> result().
not_implemented_method(Command, _Type, State) ->
  lager:warning("[Command not implemented] [command=~s]", [Command]),
  Error = <<"Command not implemented: ", Command/binary>>,
  {error_response, Error, State}.
