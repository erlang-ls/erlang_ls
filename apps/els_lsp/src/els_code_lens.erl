%%==============================================================================
%% Code Lens: Behaviour and API
%%==============================================================================

-module(els_code_lens).

%%==============================================================================
%% Callback Functions
%%==============================================================================

-callback init(els_dt_document:item()) -> state().
-callback command(els_dt_document:item(), poi(), state()) ->
  els_command:command().
-callback is_default() -> boolean().
-callback pois(els_dt_document:item()) -> [poi()].
-callback precondition(els_dt_document:item()) -> boolean().
-optional_callbacks([ init/1
                    , precondition/1
                    ]).

%%==============================================================================
%% API
%%==============================================================================

-export([ available_lenses/0
        , default_lenses/0
        , enabled_lenses/0
        , lenses/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type lens() :: #{ range   := range()
                 , command => els_command:command()
                 , data    => any()
                 }.
-type lens_id() :: binary().
-type state() :: any().
-export_type([ lens/0
             , lens_id/0
             , state/0
             ]).

%%==============================================================================
%% API
%%==============================================================================

-spec available_lenses() -> [lens_id()].
available_lenses() ->
  [ <<"ct-run-test">>
  , <<"server-info">>
  , <<"show-behaviour-usages">>
  , <<"suggest-spec">>
  , <<"function-references">>
  ].

-spec default_lenses() -> [lens_id()].
default_lenses() ->
  [Id || Id <- available_lenses(), (cb_module(Id)):is_default()].

-spec enabled_lenses() -> [lens_id()].
enabled_lenses() ->
  Config = els_config:get(lenses),
  Default = default_lenses(),
  Enabled = maps:get("enabled", Config, []),
  Disabled = maps:get("disabled", Config, []),
  lists:usort((Default ++ valid(Enabled)) -- valid(Disabled)).

-spec lenses(lens_id(), els_dt_document:item()) -> [lens()].
lenses(Id, Document) ->
  CbModule = cb_module(Id),
  case precondition(CbModule, Document) of
    true ->
      State = case erlang:function_exported(CbModule, init, 1) of
                true ->
                  CbModule:init(Document);
                false ->
                  'state_not_initialized'
              end,
      [make_lens(CbModule, Document, POI, State) ||
        POI <- CbModule:pois(Document)];
    false ->
      []
  end.

%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_lens(atom(), els_dt_document:item(), poi(), state()) -> lens().
make_lens(CbModule, Document, #{range := Range} = POI, State) ->
  #{ range   => els_protocol:range(Range)
   , command => CbModule:command(Document, POI, State)
   , data    => []
   }.

%% @doc Return the callback module for a given Code Lens Identifier
-spec cb_module(lens_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"els_code_lens_", Id/binary>>, utf8).

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec is_valid(lens_id()) -> boolean().
is_valid(Id) ->
  lists:member(Id, available_lenses()).

-spec valid([string()]) -> [lens_id()].
valid(Ids0) ->
  Ids = [els_utils:to_binary(Id) || Id <- Ids0],
  {Valid, Invalid} = lists:partition(fun is_valid/1, Ids),
  case Invalid of
    [] ->
      ok;
    _ ->
      Fmt = "Skipping invalid lenses in config file: ~p",
      Args = [Invalid],
      Msg = lists:flatten(io_lib:format(Fmt, Args)),
      ?LOG_WARNING(Msg),
      els_server:send_notification(<<"window/showMessage">>,
                                   #{ type => ?MESSAGE_TYPE_WARNING,
                                      message => els_utils:to_binary(Msg)
                                    })
  end,
  Valid.

-spec precondition(atom(), els_dt_document:item()) -> boolean().
precondition(CbModule, Document) ->
  case erlang:function_exported(CbModule, precondition, 1) of
    true ->
      CbModule:precondition(Document);
    false ->
      true
  end.
