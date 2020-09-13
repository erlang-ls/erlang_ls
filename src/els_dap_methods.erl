-module(els_dap_methods).

-include("erlang_ls.hrl").

-export([ dispatch/4
        ]).

-export([ initialize/2
        , configuration_done/2
        , set_breakpoints/2
        , set_exception_breakpoints/2
        , threads/2
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
do_dispatch(<<"initialize">>, Args, State) ->
  initialize(Args, State);
do_dispatch(<<"launch">>, Args, State) ->
  launch(Args, State);
do_dispatch(<<"setBreakpoints">>, Args, State) ->
  set_breakpoints(Args, State);
do_dispatch(<<"setExceptionBreakpoints">>, Args, State) ->
  set_exception_breakpoints(Args, State);
do_dispatch(<<"configurationDone">>, Args, State) ->
  configuration_done(Args, State);
do_dispatch(<<"threads">>, Args, State) ->
  threads(Args, State);
do_dispatch(Command, Args, #{status := initialized} = State) ->
  Function = binary_to_atom(Command, utf8),
  els_dap_methods:Function(Args, State);
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

%%==============================================================================
%% Initialize
%%==============================================================================

-spec initialize(params(), state()) -> result().
initialize(Params, State) ->
  Provider = els_dap_general_provider,
  Request  = {initialize, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State}.

%%==============================================================================
%% Launch
%%==============================================================================

-spec launch(params(), state()) -> result().
launch(Params, State) ->
  Provider = els_dap_general_provider,
  Request  = {launch, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State#{status => initialized}}.

%%==============================================================================
%% configurationDone
%%==============================================================================

-spec configuration_done(params(), state()) -> result().
configuration_done(Params, State) ->
  Provider = els_dap_general_provider,
  Request  = {configuration_done, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State}.

%%==============================================================================
%% setBreakpoints
%%==============================================================================

-spec set_breakpoints(params(), state()) -> result().
set_breakpoints(Params, State) ->
  Provider = els_dap_general_provider,
  Request  = {set_breakpoints, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State}.

%%==============================================================================
%% setExceptionBreakpoints
%%==============================================================================

-spec set_exception_breakpoints(params(), state()) -> result().
set_exception_breakpoints(Params, State) ->
  Provider = els_dap_general_provider,
  Request  = {set_exception_breakpoints, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State}.

%%==============================================================================
%% threads
%%==============================================================================

-spec threads(params(), state()) -> result().
threads(Params, State) ->
  Provider = els_dap_general_provider,
  Request  = {threads, Params},
  Response = els_provider:handle_request(Provider, Request),
  {response, Response, State}.
