%%==============================================================================
%% Code Lens: Behaviour and API
%%==============================================================================

-module(els_code_lens).

%%==============================================================================
%% Callback Functions
%%==============================================================================

-callback command() -> els_command:command_id().
-callback is_default() -> boolean().
-callback lenses(els_dt_document:item()) -> [lens()].

%%==============================================================================
%% API
%%==============================================================================

-export([ available_lenses/0
        , default_lenses/0
        , enabled_lenses/0
        , lenses/2
        ]).

%%==============================================================================
%% Constructors
%%==============================================================================

-export([ make_lens/3 ]).

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
  [<<"server-info">>].

-spec default_lenses() -> [lens_id()].
default_lenses() ->
  [Id || Id <- available_lenses(), (cb_module(Id)):is_default()].

-spec enabled_lenses() -> [lens_id()].
enabled_lenses() ->
  els_config:get(code_lenses).

-spec lenses(lens_id(), els_dt_document:item()) -> [lens()].
lenses(Id, Document) ->
  CbModule = cb_module(Id),
  CbModule:lenses(Document).

%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_lens(range(), els_command:command(), any()) -> lens().
make_lens(Range, Command, Data) ->
  #{ range   => Range
   , command => Command
   , data    => Data
   }.

%% @doc Return the callback module for a given Code Lens Identifier
-spec cb_module(els_code_lens:lens_id()) -> module().
cb_module(Id0) ->
  Id = re:replace(Id0, "-", "_", [global, {return, binary}]),
  binary_to_atom(<<"els_code_lens_", Id/binary>>, utf8).
