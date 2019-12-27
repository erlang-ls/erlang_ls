-module(els_implementation_provider).

-behaviour(els_provider).

-include("erlang_ls.hrl").

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(tuple(), els_provider:state()) ->
   {null | location(), els_provider:state()}.
handle_request({implementation, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  case find_implementation(Document, Line, Character) of
    [#{range := Range}|_] ->
      {#{uri => Uri, range => els_protocol:range(Range)}, State};
    [] ->
      {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec find_implementation( els_dt_document:item()
                         , non_neg_integer()
                         , non_neg_integer()
                         ) -> [poi()].
find_implementation(Document, Line, Character) ->
  case els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1) of
    [POI|_] -> implementation(Document, POI);
    []      -> []
  end.

-spec implementation(els_dt_document:item(), poi()) -> [poi()].
implementation(Document, #{kind := application, id := MFA}) ->
  case callback(MFA) of
    {CF, CA} ->
      POIs = els_dt_document:pois(Document, [function]),
      [POI || #{id := {F, A}} = POI <- POIs, F =:= CF, A =:= CA];
    undefined ->
      []
  end;
implementation(_Document, _POI) ->
  [].

-spec callback(mfa()) -> {atom(), non_neg_integer()} | undefined.
%% gen_event
callback({gen_event, add_handler, 3})     -> {init, 1};
callback({gen_event, add_sup_handler, 3}) -> {init, 1};
callback({gen_event, call, 3})            -> {handle_call, 2};
callback({gen_event, call, 4})            -> {handle_call, 2};
callback({gen_event, delete_handler, 1})  -> {terminate, 2};
callback({gen_event, notify, 2})          -> {handle_event, 2};
callback({gen_event, sync_notify, 2})     -> {handle_event, 2};
callback({gen_event, stop, 1})            -> {terminate, 2};
callback({gen_event, stop, 3})            -> {terminate, 2};
%% gen_server
callback({gen_server, abcast, 2})         -> {handle_cast, 2};
callback({gen_server, abcast, 3})         -> {handle_cast, 2};
callback({gen_server, call, 2})           -> {handle_call, 3};
callback({gen_server, call, 3})           -> {handle_call, 3};
callback({gen_server, cast, 2})           -> {handle_cast, 2};
callback({gen_server, multi_call, 2})     -> {handle_call, 3};
callback({gen_server, multi_call, 3})     -> {handle_call, 3};
callback({gen_server, multi_call, 4})     -> {handle_call, 3};
callback({gen_server, start, 3})          -> {init, 1};
callback({gen_server, start, 4})          -> {init, 1};
callback({gen_server, start_link, 3})     -> {init, 1};
callback({gen_server, start_link, 4})     -> {init, 1};
callback({gen_server, stop, 1})           -> {terminate, 2};
callback({gen_server, stop, 3})           -> {terminate, 2};
%% gen_statem
callback({gen_statem, call, 2})           -> {handle_event, 4};
callback({gen_statem, call, 3})           -> {handle_event, 4};
callback({gen_statem, cast, 2})           -> {handle_event, 4};
callback({gen_statem, start, 3})          -> {init, 1};
callback({gen_statem, start, 4})          -> {init, 1};
callback({gen_statem, start_link, 3})     -> {init, 1};
callback({gen_statem, start_link, 4})     -> {init, 1};
callback({gen_statem, stop, 1})           -> {terminate, 3};
callback({gen_statem, stop, 3})           -> {terminate, 3};
%% supervisor
callback({supervisor, start_link, 2})     -> {init, 1};
callback({supervisor, start_link, 3})     -> {init, 1};
%% Everything else
callback(_)                               -> undefined.
