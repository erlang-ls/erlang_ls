%%==============================================================================
%% A JSON-RPC Helper Library
%%==============================================================================
-module(els_jsonrpc).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ default_opts/0
        , split/1
        , split/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_core.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec split(binary()) -> {[map()], binary()}.
split(Data) ->
  split(Data, default_opts()).

-spec split(binary(), [any()]) -> {[map()], binary()}.
split(Data, DecodeOpts) ->
  split(Data, DecodeOpts, []).

-spec split(binary(), [any()], [map()]) -> {[map()], binary()}.
split(Data, DecodeOpts, Responses) ->
  try cow_http:parse_headers(Data) of
    {Headers, Data1} ->
      BinLength     = proplists:get_value(<<"content-length">>, Headers),
      Length        = binary_to_integer(BinLength),
      CurrentLength = byte_size(Data1),
      case CurrentLength < Length of
        true  ->
          {lists:reverse(Responses), Data};
        false ->
          <<Body:Length/binary, Rest/binary>> = Data1,
          Response  = jsx:decode(Body, DecodeOpts),
          split(Rest, DecodeOpts, [Response|Responses])
      end
  catch _:_ ->
      {lists:reverse(Responses), Data}
  end.

-spec default_opts() -> [any()].
default_opts() ->
  [return_maps, {labels, atom}].
