-module(els_completion_provider).

-behaviour(els_provider).

-include("erlang_ls.hrl").

-export([ handle_request/2
        , is_enabled/0
        ]).

%% Exported to ease testing.
-export([ keywords/0 ]).

-type options() :: #{ trigger  := binary()
                    , document := els_dt_document:item()
                    , line     := line()
                    , column   := column()
                    }.

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
handle_request({completion, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, #{text := Text} = Document} = els_utils:lookup_document(Uri),
  case maps:find(<<"context">>, Params) of
    {ok, Context} ->
      TriggerKind = maps:get(<<"triggerKind">>, Context),
      TriggerCharacter = maps:get(<<"triggerCharacter">>, Context, <<>>),
      %% We subtract 1 to strip the character that triggered the
      %% completion from the string.
      Length = case Character > 0 of true -> 1; false -> 0 end,
      Prefix = case TriggerKind of
                 ?COMPLETION_TRIGGER_KIND_CHARACTER ->
                   els_text:line(Text, Line, Character - Length);
                 ?COMPLETION_TRIGGER_KIND_INVOKED ->
                   els_text:line(Text, Line, Character)
               end,
      Opts   = #{ trigger  => TriggerCharacter
                , document => Document
                , line     => Line + 1
                , column   => Character
                },
      {find_completion(Prefix, TriggerKind, Opts), State};
    error ->
      {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_completion(binary(), integer(), options()) -> any().
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{ trigger  := <<":">>
                  , document := Document
                  , line     := Line
                  , column   := Column
                  }
               ) ->
  case els_text:last_token(Prefix) of
    {atom, _, Module} ->
      {ExportFormat, TypeOrFun} = function_context(Document, Line, Column),
      exported_functions(Module, TypeOrFun, ExportFormat);
    _ ->
      null
  end;
find_completion( _Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<"?">>, document := Document}
               ) ->
  definitions(Document, define);
find_completion( _Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<"#">>, document := Document}
               ) ->
  definitions(Document, record);
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<".">>, document := Document}
               ) ->
  case lists:reverse(els_text:tokens(Prefix)) of
    [{atom, _, RecordName}, {'#', _} | _] ->
      record_fields(Document, RecordName);
    _ ->
      []
    end;
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_INVOKED
               , #{ document := Document
                  , line     := Line
                  , column   := Column
                  }
               ) ->
  case lists:reverse(els_text:tokens(Prefix)) of
    %% Check for "[...] fun atom:atom"
    [{atom, _, _}, {':', _}, {atom, _, Module}, {'fun', _} | _] ->
      exported_functions(Module, function, _ExportFormat = true);
    %% Check for "[...] atom:atom"
    [{atom, _, _}, {':', _}, {atom, _, Module} | _] ->
      {ExportFormat, TypeOrFun} = function_context(Document, Line, Column),
      exported_functions(Module, TypeOrFun, ExportFormat);
    %% Check for "[...] ?anything"
    [_, {'?', _} | _] ->
      definitions(Document, define);
    %% Check for "[...] #anything.something"
    [_, {'.', _}, {atom, _, RecordName}, {'#', _} | _] ->
      record_fields(Document, RecordName);
    %% Check for "[...] #anything"
    [_, {'#', _} | _] ->
      definitions(Document, record);
    %% Check for "[...] Variable"
    [{var, _, _} | _] ->
      variables(Document);
    %% Check for "[...] fun atom"
    [{atom, _, _}, {'fun', _} | _] ->
      functions(Document, function, _ExportFormat = true);
    %% Check for "[...] atom"
    [{atom, _, Name} | _] ->
      NameBinary = atom_to_binary(Name, utf8),
      {ExportFormat, TypeOrFun} = function_context(Document, Line, Column),
      keywords()
      ++ modules(NameBinary)
      ++ functions(Document, TypeOrFun, ExportFormat);
    _ ->
      []
  end;
find_completion(_Prefix, _TriggerKind, _Opts) ->
  null.

%%==============================================================================
%% Modules
%%==============================================================================

-spec modules(binary()) -> [map()].
modules(Prefix) ->
  {ok, Items} = els_dt_document_index:find_by_kind(module),
  Modules = [Id || #{id := Id} <- Items],
  filter_by_prefix(Prefix, Modules, fun to_binary/1, fun item_kind_module/1).

-spec item_kind_module(binary()) -> map().
item_kind_module(Module) ->
  #{ label            => Module
   , kind             => ?COMPLETION_ITEM_KIND_MODULE
   , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
   }.

%%==============================================================================
%% Functions/Types
%%==============================================================================

-type function_type() :: function | type.

-spec functions(els_dt_document:item(), function_type(), boolean()) -> [map()].
functions(Document, Type, ExportFormat) ->
  functions(Document, Type, ExportFormat, _ExportedOnly = false).

-spec functions( els_dt_document:item()
               , function_type()
               , boolean()
               , boolean()
               ) -> [map()].
functions(Document, Type, ExportFormat, ExportedOnly) ->
  {ExportKind, ItemKind, Kind} =
    case Type of
        type ->
          { export_type_entry
          , type_definition
          , ?COMPLETION_ITEM_KIND_TYPE_PARAM
          };
        function ->
          { export_entry
          , function
          , ?COMPLETION_ITEM_KIND_FUNCTION
          }
      end,
  Exports  = els_dt_document:pois(Document, [ExportKind]),
  POIs     = els_dt_document:pois(Document, [ItemKind]),
  FAs      = [FA || #{id := FA} <- Exports],
  Items    = resolve_functions(POIs, FAs, ExportedOnly, ExportFormat, Kind),
  lists:usort(Items).

-spec function_context(els_dt_document:item(), line(), column()) ->
  {boolean(), function_type()}.
function_context(Document, Line, Column) ->
  ExportFormat = is_in(Document, Line, Column, [export, export_type]),
  TypeOrFun    = case is_in(Document, Line, Column, [spec, export_type]) of
                   true -> type;
                   false -> function
                 end,
  {ExportFormat, TypeOrFun}.

-spec resolve_functions( [poi()], [{atom(), arity()}], boolean()
                       , boolean(), completion_item_kind()) ->
  [map()].
resolve_functions(Functions, ExportsFA, ExportedOnly, ArityOnly, ItemKind) ->
  [ completion_item_with_args(POI, ItemKind, ArityOnly)
    || #{id := FA} = POI <- Functions,
        not ExportedOnly orelse lists:member(FA, ExportsFA)
  ].

-spec completion_item_with_args(poi(), completion_item_kind(), boolean()) ->
  map().
completion_item_with_args(#{id := {F, A}, data := ArgsNames}, Kind, false) ->
  #{ label            => list_to_binary(io_lib:format("~p/~p", [F, A]))
   , kind             => Kind
   , insertText       => snippet_function_call(F, ArgsNames)
   , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
   };
completion_item_with_args(#{id := {F, A}}, Kind, true) ->
  #{ label            => list_to_binary(io_lib:format("~p/~p", [F, A]))
   , kind             => Kind
   , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
   }.

-spec exported_functions(module(), function_type(), boolean()) ->
  [map()].
exported_functions(Module, TypeOrFunction, ExportFormat) ->
  case els_utils:find_module(Module) of
    {ok, Uri} ->
      {ok, Document} = els_utils:lookup_document(Uri),
      functions(Document, TypeOrFunction, ExportFormat, true);
    {error, _Error} ->
      []
  end.

-spec snippet_function_call(atom(), [{integer(), string()}]) -> binary().
snippet_function_call(Function, Args0) ->
  Args    = [ ["${", integer_to_list(N), ":", A, "}"]
              || {N, A} <- Args0
            ],
  Snippet = [atom_to_list(Function), "(", string:join(Args, ", "), ")"],
  iolist_to_binary(Snippet).

-spec is_in(els_dt_document:item(), line(), column(), [poi_kind()]) ->
  boolean().
is_in(Document, Line, Column, POIKinds) ->
  POIs = els_dt_document:get_element_at_pos(Document, Line, Column),
  IsKind = fun(#{kind := Kind}) -> lists:member(Kind, POIKinds) end,
  lists:any(IsKind, POIs).

%%==============================================================================
%% Variables
%%==============================================================================

-spec variables(els_dt_document:item()) -> [map()].
variables(Document) ->
  POIs = els_dt_document:pois(Document, [variable]),
  Vars = [ #{ label => atom_to_binary(Name, utf8)
            , kind  => ?COMPLETION_ITEM_KIND_VARIABLE
            }
           || #{id := Name} <- POIs
         ],
  lists:usort(Vars).

%%==============================================================================
%%  Record Fields
%%==============================================================================

-spec record_fields(els_dt_document:item(), atom()) -> [map()].
record_fields(Document, RecordName) ->
  case find_record_definition(Document, RecordName) of
    [] -> [];
    POIs ->
      [#{data := Fields} | _] = els_poi:sort(POIs),
      [ #{ label => atom_to_binary(Name, utf8)
         , kind  => ?COMPLETION_ITEM_KIND_FIELD
         }
        || {Name, _} <- Fields
      ]
  end.

-spec find_record_definition(els_dt_document:item(), atom()) -> [poi()].
find_record_definition(Document, RecordName) ->
  POIs = lists:flatten([ local_definitions(Document, record)
                       , included_definitions(Document, record)
                       ]),
  [X || X = #{id := Name} <- POIs, Name =:= RecordName].

%%==============================================================================
%% Macros and Records
%%==============================================================================

-type def_type() :: define | record.

-spec definitions(els_dt_document:item(), def_type()) -> [map()].
definitions(Document, Type) ->
  POIs = lists:flatten([ local_definitions(Document, Type)
                       , included_definitions(Document, Type)
                       ]),
  Defs = [ #{ label => atom_to_binary(Name, utf8)
            , kind  => completion_item_kind(Type)
            }
           || #{id := Name} <- POIs
         ],
  lists:usort(Defs).

-spec local_definitions(els_dt_document:item(), def_type()) -> [map()].
local_definitions(Document, Type) ->
  els_dt_document:pois(Document, [Type]).

-spec included_definitions(els_dt_document:item(), def_type()) -> [[map()]].
included_definitions(Document, Type) ->
  POIs  = els_dt_document:pois(Document, [include, include_lib]),
  [include_file_definitions(Name, Type) || #{id := Name} <- POIs].

-spec include_file_definitions(string(), def_type()) -> [map()].
include_file_definitions(Name, Type) ->
  Filename = filename:basename(Name, filename:extension(Name)),
  H = list_to_atom(Filename),
  case els_utils:find_header(H) of
    {ok, Uri} ->
      {ok, IncludeDocument} = els_utils:lookup_document(Uri),
      local_definitions(IncludeDocument, Type);
    {error, _} ->
      []
  end.

%%==============================================================================
%% Keywords
%%==============================================================================

-spec keywords() -> [map()].
keywords() ->
  Keywords = [ 'after', 'and', 'andalso', 'band', 'begin', 'bnot', 'bor', 'bsl'
             , 'bsr', 'bxor', 'case', 'catch', 'cond', 'div', 'end', 'fun'
             , 'if', 'let', 'not', 'of', 'or', 'orelse', 'receive', 'rem'
             , 'try', 'when', 'xor'],
  [ #{ label => atom_to_binary(K, utf8)
     , kind  => ?COMPLETION_ITEM_KIND_KEYWORD
     } || K <- Keywords ].

%%==============================================================================
%% Filter by prefix
%%==============================================================================

%% TODO: Implement as select
-spec filter_by_prefix(binary(), [binary()], function(), function()) -> [map()].
filter_by_prefix(Prefix, List, ToBinary, ItemFun) ->
  FilterMapFun = fun(X) ->
                     Str = ToBinary(X),
                     case string:prefix(Str, Prefix)  of
                       nomatch -> false;
                       _       -> {true, ItemFun(Str)}
                     end
                 end,
  lists:filtermap(FilterMapFun, List).

-spec to_binary(any()) -> binary().
to_binary(X) when is_atom(X) ->
  atom_to_binary(X, utf8);
to_binary(X) when is_binary(X) ->
  X.

%%==============================================================================
%% Maps definition types to completion item kinds
%%==============================================================================

-spec completion_item_kind(def_type()) -> completion_item_kind().
completion_item_kind(define) ->
  ?COMPLETION_ITEM_KIND_CONSTANT;
completion_item_kind(record) ->
  ?COMPLETION_ITEM_KIND_STRUCT.
