%%==============================================================================
%% Behaviour and API to report diagnostics
%%==============================================================================
-module(els_diagnostics).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

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
-callback is_default()                       -> boolean().
-callback run(uri())                         -> [diagnostic()].
-callback source()                           -> binary().
-callback on_complete(uri(), [diagnostic()]) -> ok.
-optional_callbacks([ on_complete/2 ]).

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
  [ <<"bound_var_in_pattern">>
  , <<"compiler">>
  , <<"crossref">>
  , <<"dialyzer">>
  , <<"gradualizer">>
  , <<"elvis">>
  , <<"unused_includes">>
  , <<"unused_macros">>
  , <<"unused_record_fields">>
  ].

-spec default_diagnostics() -> [diagnostic_id()].
default_diagnostics() ->
  [Id || Id <- available_diagnostics(), (cb_module(Id)):is_default()].

-spec enabled_diagnostics() -> [diagnostic_id()].
enabled_diagnostics() ->
  Config = els_config:get(diagnostics),
  Default = default_diagnostics(),
  Enabled = maps:get("enabled", Config, []),
  Disabled = maps:get("disabled", Config, []),
  lists:usort((Default ++ valid(Enabled)) -- valid(Disabled)).

-spec make_diagnostic(range(), binary(), severity(), binary()) -> diagnostic().
make_diagnostic(Range, Message, Severity, Source) ->
  #{ range    => Range
   , message  => Message
   , severity => Severity
   , source   => Source
   }.

-spec run_diagnostics(uri()) -> [pid()].
run_diagnostics(Uri) ->
  [run_diagnostic(Uri, Id) || Id <- els_config:get(enabled_diagnostics)].

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec run_diagnostic(uri(), diagnostic_id()) -> pid().
run_diagnostic(Uri, Id) ->
  CbModule = cb_module(Id),
  Source = CbModule:source(),
  Module = atom_to_binary(els_uri:module(Uri), utf8),
  Title = <<Source/binary, " (", Module/binary, ")">>,
  Config = #{ task => fun(U, _) -> CbModule:run(U) end
            , entries => [Uri]
            , title => Title
            , on_complete =>
                fun(Diagnostics) ->
                    case erlang:function_exported(CbModule, on_complete, 2) of
                      true ->
                        CbModule:on_complete(Uri, Diagnostics);
                      false ->
                        ok
                    end,
                    els_diagnostics_provider:notify(Diagnostics, self())
                end
            },
  {ok, Pid} = els_background_job:new(Config),
  Pid.

%% @doc Return the callback module for a given Diagnostic Identifier
-spec cb_module(diagnostic_id()) -> module().
cb_module(Id) ->
  binary_to_existing_atom(<<"els_", Id/binary, "_diagnostics">>, utf8).

-spec is_valid(diagnostic_id()) -> boolean().
is_valid(Id) ->
  lists:member(Id, available_diagnostics()).

-spec valid([string()]) -> [diagnostic_id()].
valid(Ids0) ->
  Ids = [els_utils:to_binary(Id) || Id <- Ids0],
  {Valid, Invalid} = lists:partition(fun is_valid/1, Ids),
  case Invalid of
    [] ->
      ok;
    _ ->
      Fmt = "Skipping invalid diagnostics in config file: ~p",
      Args = [Invalid],
      Msg = lists:flatten(io_lib:format(Fmt, Args)),
      ?LOG_WARNING(Msg),
      els_server:send_notification(<<"window/showMessage">>,
                                   #{ type => ?MESSAGE_TYPE_WARNING,
                                      message => els_utils:to_binary(Msg)
                                    })
  end,
  Valid.
