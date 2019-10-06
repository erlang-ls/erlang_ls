-module(erlang_ls_references_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

-define(REFERENCES_INDEX, references_index).

-type key() :: {module(), atom()}.
-type ref() :: #{uri := erlang_ls_uri:uri(), range := erlang_ls_poi:range()}.

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  Paths = erlang_ls_code_navigation:include_path(),
  ets:new(?REFERENCES_INDEX, [named_table, public, set]),
  erlang:spawn(fun() -> index(Paths) end),
  ok.

-spec teardown() -> ok.
teardown() ->
  ok.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({definition, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  case erlang_ls_buffer:get_element_at_pos(Buffer, Line + 1, Character + 1) of
    [POI | _] ->
      Filename = erlang_ls_uri:path(Uri),
      {find_references(Filename, POI), State};
    [] -> {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_references(binary(), erlang_ls_poi:poi()) -> any().
find_references(Filename, #{info := {Type, MFA}})
  when Type =:= application; Type =:= implicit_fun; Type =:= function ->
  Key = key(Filename, MFA),
  case references(Key) of
    [] -> null;
    Locations -> Locations
  end;
find_references(_, _) ->
  null.

-spec references(key()) -> [any()].
references(Key) ->
  [location(U, R) || #{uri := U, range := R} <- get_refs(Key)].

-spec location(erlang_ls_uri:uri(), erlang_ls_poi:range()) -> map().
location(Uri, Range) ->
  #{ uri   => Uri
   , range => erlang_ls_protocol:range(Range)
   }.

-spec index([string()]) -> ok.
index(Paths) ->
  [ filelib:fold_files(Path, ".*\\.erl", true, fun index_file/2, ok)
    || Path <- Paths
  ],
  ok.

-spec index_file(string(), ok) -> ok.
index_file(File, _Acc) ->
  try
    {ok, Tree, Extra} = erlang_ls_parser:parse_file(File),
    AnnotatedTree = erlang_ls_tree:annotate(Tree, Extra),
    Uri = erlang_ls_uri:uri(erlang:iolist_to_binary(File)),
    [ register_applications(Uri, POI)
      || POI <- erlang_ls_poi:list(AnnotatedTree)
    ],
    ok
  catch Type:Reason:St ->
      lager:error("Error indexing ~s: ~p", [File, Reason]),
      erlang:raise(Type, Reason, St)
  end.

-spec register_applications(erlang_ls_uri:uri(), erlang_ls_poi:poi()) -> ok.
register_applications(Uri, #{info := {Type, MFA}, range := Range})
  when Type =:= application; Type =:= implicit_fun ->
  Key = key(Uri, MFA),
  Ref = #{uri => Uri, range => Range},
  add_ref(Key, Ref),
  ok;
register_applications(_File, _POI) ->
  ok.

-spec key(binary(), {module(), atom(), arity()} | {atom(), arity()}) ->
  key().
key(_Filename, {M, F, _A}) ->
  {M, F};
key(Filename, {F, _A}) ->
  Extension = filename:extension(Filename),
  Basename = filename:basename(Filename, Extension),
  Module = binary_to_atom(Basename, utf8),
  {Module, F}.

-spec get_refs(key()) ->[ref()].
get_refs(Key) ->
  case ets:lookup(?REFERENCES_INDEX, Key) of
    [] -> [];
    [{Key, Refs}] -> Refs
  end.

-spec add_ref(key(), ref()) -> ok.
add_ref(Key, Value) ->
  Refs = get_refs(Key),
  true = ets:insert(?REFERENCES_INDEX, {Key, [Value | Refs]}),
  ok.
