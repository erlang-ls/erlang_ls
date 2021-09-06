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
%% Types
%%==============================================================================
-type more()   :: {more, undefined | non_neg_integer()}.
-type header() :: {atom() | binary(), binary()}.

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
  case peel_content(Data) of
    {ok, Body, Rest} ->
      Response = jsx:decode(Body, DecodeOpts),
      split(Rest, DecodeOpts, [Response|Responses]);
    {more, _Length} ->
      {lists:reverse(Responses), Data}
  end.

-spec peel_content(binary()) -> {ok, binary(), binary()} | more().
peel_content(Data) ->
  case peel_headers(Data) of
    {ok, Headers, Data1} ->
      BinLength     = proplists:get_value('Content-Length', Headers),
      Length        = binary_to_integer(BinLength),
      case Data1 of
        <<Body:Length/binary, Rest/binary>> ->
          {ok, Body, Rest};
        Data1 ->
          {more, Length - byte_size(Data1)}
      end;
    {more, Length} ->
      {more, Length}
  end.

-spec peel_headers(binary()) -> {ok, [header()], binary()} | more().
peel_headers(Data) ->
  peel_headers(Data, []).

-spec peel_headers(binary(), [header()]) -> {ok, [header()], binary()} | more().
peel_headers(Data, Headers) ->
  case erlang:decode_packet(httph_bin, Data, []) of
    {ok, http_eoh, Rest} ->
      {ok, lists:reverse(Headers), Rest};
    {ok, {http_header, _Bit, Field, _UnmodifiedField, Value}, Rest} ->
      peel_headers(Rest, [{Field, Value}|Headers]);
    {more, Length} ->
      {more, Length};
    {error, Reason} ->
      erlang:error(Reason, [Data, Headers])
  end.

-spec default_opts() -> [any()].
default_opts() ->
  [return_maps, {labels, atom}].
