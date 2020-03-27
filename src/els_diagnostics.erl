%%==============================================================================
%% Behaviour and API to report diagnostics
%%==============================================================================
-module(els_diagnostics).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type diagnostic() :: #{ range              := range()
                       , severity           => severity()
                       , code               => number() | binary()
                       , source             => binary()
                       , message            := binary()
                       , relatedInformation => [related_info()]
                       }.
-type diagnostic_id() :: binary().
-type related_info() :: #{ location := location()
                         , message  := binary()
                         }.
-type severity() :: ?DIAGNOSTIC_ERROR
                  | ?DIAGNOSTIC_WARNING
                  | ?DIAGNOSTIC_INFO
                  | ?DIAGNOSTIC_HINT.
-export_type([ diagnostic/0
             , diagnostic_id/0
             , severity/0
             ]).

%%==============================================================================
%% Callback Functions Definitions
%%==============================================================================
-callback is_default() -> boolean().
-callback run(uri())   -> [diagnostic()].
-callback source()     -> binary().

%%==============================================================================
%% API
%%==============================================================================
-export([ available_diagnostics/0
        , default_diagnostics/0
        , enabled_diagnostics/0
        , make_diagnostic/4
        , run_diagnostics/1
        ]).

%%==============================================================================
%% API
%%==============================================================================

-spec available_diagnostics() -> [diagnostic_id()].
available_diagnostics() ->
  [ <<"compiler">>
  , <<"dialyzer">>
  , <<"elvis">>
  ].

-spec default_diagnostics() -> [diagnostic_id()].
default_diagnostics() ->
  [Id || Id <- available_diagnostics(), (cb_module(Id)):is_default()].

-spec enabled_diagnostics() -> [diagnostic_id()].
enabled_diagnostics() ->
  els_config:get(diagnostics).

-spec make_diagnostic(range(), binary(), severity(), binary()) -> diagnostic().
make_diagnostic(Range, Message, Severity, Source) ->
  #{ range    => Range
   , message  => Message
   , severity => Severity
   , source   => Source
   }.

-spec run_diagnostics(uri()) -> [pid()].
run_diagnostics(Uri) ->
  [run_diagnostic(Uri, Id) || Id <- enabled_diagnostics()].

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec run_diagnostic(uri(), diagnostic_id()) -> pid().
run_diagnostic(Uri, Id) ->
  CbModule = cb_module(Id),
  Config = #{ task => fun(U, _) -> CbModule:run(U) end
            , entries => [Uri]
            , title => CbModule:source()
            , on_complete =>
                fun(Diagnostics) ->
                    els_diagnostics_provider:notify(Diagnostics, self())
                end
            },
  {ok, Pid} = els_background_job:new(Config),
  Pid.

%% @doc Return the callback module for a given Diagnostic Identifier
-spec cb_module(els_diagnostics:diagnostic_id()) -> module().
cb_module(Id) ->
  binary_to_atom(<<"els_", Id/binary, "_diagnostics">>, utf8).
