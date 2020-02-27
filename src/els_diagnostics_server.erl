%%==============================================================================
%% Diagnostics Server
%%==============================================================================
-module(els_diagnostics_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ start_link/2
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%% API
-export([ on_save/1
        , on_open/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { server_name :: atom()
               , uri :: uri()
               , diagnostics :: term()
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link({atom(), uri()}, term()) -> {ok, pid()}.
start_link({Id, Uri}, _Opts) ->
  {ok, _} = gen_server:start_link({local, Id},
                                  ?MODULE,
                                  {Id, Uri},
                                  []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init({atom(), uri()}) -> {ok, state()}.
init({Id, Uri}) ->
  State = #state{ server_name = Id
                , uri = Uri
                },
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(get_diagnostics, _From, #state{diagnostics = Diagnostics} = S) ->
  {reply, Diagnostics, S};
handle_call(UnknownMessage, _From, S) ->
  lager:info("Unknown handle_call! message: ~p", [UnknownMessage]),
  {reply, ok, S}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(run, #state{uri = Uri} = S) ->
  Diagnostics =
    lists:foldl(
      fun(Mod, AccumulatedDiagnostics0) ->
          Diagnostics = diagnostic(Mod, Uri),
          AccumulatedDiagnostics = AccumulatedDiagnostics0 ++ Diagnostics,
          send_notification(Uri, AccumulatedDiagnostics),
          AccumulatedDiagnostics
      end, [], [ els_compiler_diagnostics
               , els_elvis_diagnostics
               , els_dialyzer_diagnostics
               ]),
  {noreply, S#state{diagnostics = Diagnostics}};
handle_cast(UnknownMessage, S) ->
  lager:info("Unknown handle_cast! message: ~p", [UnknownMessage]),
  {noreply, S}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'EXIT', _, normal}, S) ->
  {noreply, S};
handle_info(UnknownMessage, S) ->
  lager:info("Unknown handle_info! message: ~p", [UnknownMessage]),
  {noreply, S}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec on_save(uri()) -> ok.
on_save(Uri) ->
  els_diagnostics_sup:on_save(Uri),
  Id = binary_to_atom(Uri, utf8),
  ok = gen_server:cast(Id, run).

-spec on_open(uri()) -> ok.
on_open(Uri) ->
  els_diagnostics_sup:on_open(Uri),
  Id = binary_to_atom(Uri, utf8),
  case gen_server:call(Id, get_diagnostics, 100) of
    undefined -> gen_server:cast(Id, run);
    Diagnostics -> Diagnostics
  end.

-spec diagnostic(atom(), uri()) -> [diagnostic()].
diagnostic(els_compiler_diagnostics, Uri) ->
  CDiagnostics = els_compiler_diagnostics:diagnostics(Uri),
  maybe_compile_and_load(Uri, CDiagnostics),
  CDiagnostics;
diagnostic(Module, Uri) ->
  apply(Module, diagnostics, [Uri]).

-spec send_notification(uri(), [diagnostic()]) -> ok.
send_notification(Uri, Diagnostics) ->
  Method = <<"textDocument/publishDiagnostics">>,
  Params  = #{ uri         => Uri
             , diagnostics => Diagnostics
             },
  els_server:send_notification(Method, Params).

-spec maybe_compile_and_load(uri(), [diagnostic()]) -> ok.
maybe_compile_and_load(Uri, [] = _CompilerDiagnostics) ->
  case els_config:get(code_reload) of
    #{"node" := NodeStr} ->
      Node = list_to_atom(NodeStr),
      Module = els_uri:module(Uri),
      case rpc:call(Node, code, is_sticky, [Module]) of
        true -> ok;
        _ -> handle_rpc_result(rpc:call(Node, c, c, [Module]), Module)
      end;
    disabled ->
      ok
  end;
maybe_compile_and_load(_, _) -> ok.

-spec handle_rpc_result(term() | {badrpc, term()}, atom()) -> ok.
handle_rpc_result({ok, Module}, _) ->
  Msg = io_lib:format("code_reload success for: ~s", [Module]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => list_to_binary(Msg)
                                });
handle_rpc_result(Err, Module) ->
  lager:info("[code_reload] code_reload using c:c/1 crashed with: ~p",
             [Err]),
  Msg = io_lib:format("code_reload swap crashed for: ~s with: ~w",
                      [Module, Err]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_ERROR,
                                  message => list_to_binary(Msg)
                                }).
