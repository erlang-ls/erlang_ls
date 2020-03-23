%%==============================================================================
%% Code Lens: Behaviour and API
%%==============================================================================

-module(els_code_lens).

%%==============================================================================
%% Callback Functions
%%==============================================================================

-callback command(poi()) -> els_command:command_id().
-callback command_args(els_dt_document:item(), poi()) -> [any()].
-callback is_default() -> boolean().
-callback pois(els_dt_document:item()) -> [poi()].
-callback precondition(els_dt_document:item()) -> boolean().
-callback title(poi()) -> binary().

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

-include("erlang_ls.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type lens() :: #{ range   := range()
                 , command => els_command:command()
                 , data    => any()
                 }.
-type lens_id() :: binary().
-export_type([ lens/0
             , lens_id/0
             ]).

%%==============================================================================
%% API
%%==============================================================================

-spec available_lenses() -> [lens_id()].
available_lenses() ->
  [ <<"ct-run-test">>
  , <<"server-info">>
  ].

-spec default_lenses() -> [lens_id()].
default_lenses() ->
  [Id || Id <- available_lenses(), (cb_module(Id)):is_default()].

-spec enabled_lenses() -> [lens_id()].
enabled_lenses() ->
  els_config:get(code_lenses).

-spec lenses(lens_id(), els_dt_document:item()) -> [lens()].
lenses(Id, Document) ->
  CbModule = cb_module(Id),
  case CbModule:precondition(Document) of
    true ->
      [make_lens(CbModule, Document, POI) || POI <- CbModule:pois(Document)];
    false ->
      []
  end.

%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_lens(atom(), els_dt_document:item(), poi()) -> lens().
make_lens(CbModule, Document, #{range := Range} = POI) ->
  Command = els_command:make_command( CbModule:title(POI)
                                    , CbModule:command(POI)
                                    , CbModule:command_args(Document, POI)),
  #{ range   => els_protocol:range(Range)
   , command => Command
   , data    => []
   }.

%% @doc Return the callback module for a given Code Lens Identifier
-spec cb_module(els_code_lens:lens_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"els_code_lens_", Id/binary>>, utf8).
