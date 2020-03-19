%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_compiler_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ diagnostics/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type compiler_info()  :: {erl_anno:line() | 'none', module(), any()}.
-type compiler_msg()   :: {file:filename(), [compiler_info()]}.
-type macro_config()   :: #{string() => string()}.
-type macro_option()   :: {'d', atom()} | {'d', atom(), any()}.
-type include_option() :: {'i', string()}.

%%==============================================================================
%% Callback Functions
%%==============================================================================
-spec diagnostics(uri()) -> [diagnostic()].
diagnostics(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      compile(Uri);
    <<".hrl">> ->
      %% It does not make sense to 'compile' header files in isolation
      %% (e.g. using the compile:forms/1 function). That would in fact
      %% produce a big number of false positive errors and warnings,
      %% including 'record not used' or 'module attribute not
      %% specified'. An alternative could be to use a 'fake' module
      %% that simply includes the file, but that feels a bit too
      %% hackish. As a compromise, we decided to parse the include
      %% file, since that allows us to identify most of the common
      %% errors in header files.
      parse(Uri);
    <<".escript">> ->
      parse_escript(Uri);
    _Ext ->
      lager:debug("Skipping diagnostics due to extension [uri=~p]", [Uri]),
      []
  end.

-spec source() -> binary().
source() ->
  <<"Compiler">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec compile(uri()) -> [diagnostic()].
compile(Uri) ->
  Dependencies = els_diagnostics_utils:dependencies(Uri),
  Path = unicode:characters_to_list(els_uri:path(Uri)),
  case compile_file(Path, Dependencies) of
    {ok, _, WS} ->
      diagnostics(Path, WS, ?DIAGNOSTIC_WARNING);
    {error, ES, WS} ->
      diagnostics(Path, WS, ?DIAGNOSTIC_WARNING) ++
        diagnostics(Path, ES, ?DIAGNOSTIC_ERROR)
  end.

-spec parse(uri()) -> [diagnostic()].
parse(Uri) ->
  FileName = unicode:characters_to_list(els_uri:path(Uri)),
  {ok, Epp} = epp:open([ {name, FileName}
                       , {includes, els_config:get(include_paths)}
                       ]),
  Res = [diagnostic(range(Line), Module, Desc, ?DIAGNOSTIC_ERROR)
         || {error, {Line, Module, Desc}} <- epp:parse_file(Epp)],
  epp:close(Epp),
  Res.

-spec parse_escript(uri()) -> [diagnostic()].
parse_escript(Uri) ->
  FileName = unicode:characters_to_list(els_uri:path(Uri)),
  case els_escript:extract(FileName) of
    {ok, WS} ->
      diagnostics(FileName, WS, ?DIAGNOSTIC_WARNING);
    {error, ES, WS} ->
      diagnostics(FileName, WS, ?DIAGNOSTIC_WARNING) ++
        diagnostics(FileName, ES, ?DIAGNOSTIC_ERROR)
  end.

%% @doc Convert compiler messages into diagnostics
%%
%% Convert a list of compiler messages of a given severity (warning,
%% error) into a list of diagnostic data structures, as expected by
%% the LSP protocol.
%% Compiler messages related to included files are grouped together
%% and they are presented to the user by highlighting the line where
%% the file inclusion happens.
-spec diagnostics(list(), [compiler_msg()], severity()) -> [diagnostic()].
diagnostics(Path, List, Severity) ->
  Uri = els_uri:uri(unicode:characters_to_binary(Path)),
  {ok, [Document]} = els_dt_document:lookup(Uri),
  lists:flatten([[ diagnostic( Path
                             , MessagePath
                             , range(Line)
                             , Document
                             , Module
                             , Desc
                             , Severity)
                   || {Line, Module, Desc} <- Info]
                 || {MessagePath, Info} <- List]).

-spec diagnostic( string()
                , string()
                , poi_range()
                , els_dt_document:item()
                , module()
                , string()
                , integer()) -> diagnostic().
diagnostic(Path, Path, Range, _Document, Module, Desc, Severity) ->
  %% The compiler message is related to the same .erl file, so
  %% preserve the location information.
  diagnostic(Range, Module, Desc, Severity);
diagnostic(_Path, MessagePath, Range, Document, Module, Desc0, Severity) ->
  #{from := {Line, _}} = Range,
  InclusionRange = inclusion_range(MessagePath, Document),
  %% The compiler message is related to an included file. Replace the
  %% original location with the location of the file inclusion.
  Desc = io_lib:format("Issue in included file (~p): ~p", [Line, Desc0]),
  diagnostic(InclusionRange, Module, Desc, Severity).

-spec diagnostic(poi_range(), module(), string(), integer()) ->
  diagnostic().
diagnostic(Range, Module, Desc, Severity) ->
  Message0 = lists:flatten(Module:format_error(Desc)),
  Message  = unicode:characters_to_binary(Message0),
  #{ range    => els_protocol:range(Range)
   , message  => Message
   , severity => Severity
   , source   => source()
   }.

-spec range(erl_anno:line() | none) -> poi_range().
range(Line) when is_integer(Line) ->
  #{from => {Line, 1}, to => {Line + 1, 1}};
range(none) ->
  range(1).

%% @doc Find the inclusion range for a header file.
%%
%%      Given the path of e .hrl path, find its inclusion range within
%%      a given document.
-spec inclusion_range(string(), els_dt_document:item()) -> poi_range().
inclusion_range(IncludePath, Document) ->
  [Range|_] =
    inclusion_range(IncludePath, Document, include) ++
    inclusion_range(IncludePath, Document, include_lib),
  Range.

-spec inclusion_range( string()
                     , els_dt_document:item()
                     , include | include_lib) -> [poi_range()].
inclusion_range(IncludePath, Document, include) ->
  POIs       = els_dt_document:pois(Document, [include]),
  IncludeId  = include_id(IncludePath),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= IncludeId];
inclusion_range(IncludePath, Document, include_lib) ->
  POIs       = els_dt_document:pois(Document, [include_lib]),
  IncludeId  = include_lib_id(IncludePath),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= IncludeId].

-spec include_id(string()) -> string().
include_id(Path) ->
  filename:basename(Path).

-spec include_lib_id(string()) -> string().
include_lib_id(Path) ->
  Components = filename:split(Path),
  Length     = length(Components),
  End        = Length - 1,
  Beginning  = max(1, Length - 2),
  filename:join(lists:sublist(Components, Beginning, End)).

-spec macro_options() -> [macro_option()].
macro_options() ->
  Macros = els_config:get(macros),
  [macro_option(M) || M <- Macros].

-spec macro_option(macro_config()) -> macro_option().
macro_option(#{"name" := Name, "value" := Value}) ->
  {'d', list_to_atom(Name), string_to_term(Value)};
macro_option(#{"name" := Name}) ->
  {'d', list_to_atom(Name), true}.

-spec include_options() -> [include_option()].
include_options() ->
  Paths = els_config:get(include_paths),
  [ {i, Path} || Path <- Paths ].

-spec string_to_term(list()) -> any().
string_to_term(Value) ->
  try
    {ok, Tokens, _End} = erl_scan:string(Value ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term
  catch
    _Class:Exception ->
      Fmt =
        "Error parsing custom defined macro, "
        "falling back to 'true'"
        "[value=~p] [exception=~p]",
      Args = [Value, Exception],
      lager:error(Fmt, Args),
      true
  end.

-spec compile_file(string(), [atom()]) ->
        {ok | error, [compiler_msg()], [compiler_msg()]}.
compile_file(Path, Dependencies) ->
  %% Load dependencies required for the compilation
  Olds = [load_dependency(Dependency) || Dependency <- Dependencies],
  Opts = lists:append([ macro_options()
                      , include_options()
                      , [ return_warnings
                        , return_errors
                        , basic_validation
                        ]
                      ]),
  Res = compile:file(Path, Opts),
  %% Restore things after compilation
  [code:load_binary(Dependency, Filename, Binary)
   || {Dependency, Binary, Filename} <- Olds],
  Res.

%% @doc Load a dependency, return the old version of the code (if any),
%% so it can be restored.
-spec load_dependency(atom()) -> {atom(), binary(), file:filename()} | error.
load_dependency(Module) ->
  Old = code:get_object_code(Module),
  case els_utils:find_module(Module) of
    {ok, Uri} ->
      Path = unicode:characters_to_list(els_uri:path(Uri)),
      Opts = lists:append([macro_options(), include_options(), [binary]]),
      case compile:file(Path, Opts) of
        {ok, Module, Binary} ->
          code:load_binary(Module, atom_to_list(Module), Binary);
        Error ->
          lager:warning("Error compiling dependency [error=~w]", [Error])
      end;
    {error, Error} ->
      lager:warning("Error finding dependency [error=~w]", [Error])
  end,
  Old.
