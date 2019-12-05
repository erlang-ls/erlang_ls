%%==============================================================================
%% Erlang LS TCP Client
%%==============================================================================

-module(els_stdio_client).

%%==============================================================================
%% Behaviour els_client
%%==============================================================================

-behaviour(els_client).
-export( [ start_link/1
         , send/2
         ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type args()  :: #{ io_device := any() }.

%%==============================================================================
%% Callbacks for the els_client behaviour
%%==============================================================================

-spec start_link(args()) -> {ok, pid()}.
start_link(#{io_device := IoDevice}) ->
  Args = [ []
         , IoDevice
         , fun els_client:handle_responses/1
         , els_jsonrpc:default_opts()
         ],
  _Pid = proc_lib:spawn_link(els_stdio, loop, Args),
  {ok, IoDevice}.

-spec send(pid(), iolist()) -> ok.
send(Server, Payload) ->
  io:format(Server, iolist_to_binary(Payload), []).
