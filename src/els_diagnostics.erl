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
-type related_info() :: #{ location := location()
                         , message  := binary()
                         }.
-type severity() :: ?DIAGNOSTIC_ERROR
                  | ?DIAGNOSTIC_WARNING
                  | ?DIAGNOSTIC_INFO
                  | ?DIAGNOSTIC_HINT.
-export_type([ diagnostic/0
             , severity/0
             ]).

%%==============================================================================
%% Callback Functions Definitions
%%==============================================================================
-callback diagnostics(uri()) -> [diagnostic()].
-callback source()           -> binary().

%%==============================================================================
%% API
%%==============================================================================
-export([ make_diagnostic/4
        , publish/2
        ]).

%%==============================================================================
%% API
%%==============================================================================

-spec make_diagnostic(range(), binary(), severity(), binary()) -> diagnostic().
make_diagnostic(Range, Message, Severity, Source) ->
  #{ range    => Range
   , message  => Message
   , severity => Severity
   , source   => Source
   }.

-spec publish(uri(), [diagnostic()]) -> ok.
publish(Uri, Diagnostics) ->
  Method = <<"textDocument/publishDiagnostics">>,
  Params = #{ uri         => Uri
            , diagnostics => Diagnostics
            },
  els_server:send_notification(Method, Params).
