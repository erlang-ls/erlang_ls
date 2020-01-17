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
%% Defines
%%==============================================================================
-define(COMPILER_OPTS, [ return_warnings
                       , return_errors
                       , basic_validation
                       ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type compiler_info() :: {erl_anno:line() | 'none', module(), any()}.
-type compiler_msg()  :: {file:filename(), [compiler_info()]}.
-type macro_config()  :: #{string() => string()}.
-type macro_option()  :: {'d', atom()} | {'d', atom(), any()}.

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
  Path = binary_to_list(els_uri:path(Uri)),
  Includes = [ {i, IncludePath}
               || IncludePath <- els_config:get(include_paths)
             ],
  %% Extra paths to load beam files from, for e.g. behaviours
 code:add_pathsa(els_config:get(pa_dirs)),
  Opts0 = lists:append([macro_options(), Includes]),
  Opts1 = lists:append([Opts0, ?COMPILER_OPTS]),
  case compile:file(Path, Opts1) of
    {ok, _, WS} ->
      load_if_behaviour(Path, Opts0),
      diagnostics(Path, WS, ?DIAGNOSTIC_WARNING);

    {error, ES, WS} ->
      diagnostics(Path, WS, ?DIAGNOSTIC_WARNING) ++
        diagnostics(Path, ES, ?DIAGNOSTIC_ERROR)
  end.

-spec parse(uri()) -> [diagnostic()].
parse(Uri) ->
  FileName = binary_to_list(els_uri:path(Uri)),
  Forms = do_parse(FileName),
  [diagnostic(range(Line), Module, Desc, ?DIAGNOSTIC_ERROR)
         || {error, {Line, Module, Desc}} <- Forms].

-spec do_parse(file:filename()) -> [any()].
do_parse(FileName) ->
  {ok, Epp} = epp:open([ {name, FileName}
                       , {includes, els_config:get(include_paths)}
                       ]),
  Res = [Form || Form <- epp:parse_file(Epp)],
  epp:close(Epp),
  Res.

%% @doc If a file defines a behaviour (by using the `callback' atrribute),
%% compile it to binary and load the module into the running language server, so
%% any other buffers relying on this will make use of the updated definition.
-spec load_if_behaviour(file:filename(), [compile:option()]) -> ok.
load_if_behaviour(Path, Opts) ->
  case defines_behaviour(Path) of
    true ->
      lager:debug("load_if_behaviour: true"), %% AZ
      case compile:file(Path, Opts ++ [binary]) of
        {ok, Module, B} ->
          code:load_binary(Module, Path, B);
        Oops ->
          lager:debug("load_if_behaviour:got [Oops=~p]", [Oops]), %% AZ
          ok
      end;
    false ->
      lager:debug("load_if_behaviour: false"), %% AZ
      ok
  end.

%% @doc Check if the file defines a behaviour
-spec defines_behaviour(file:filename()) -> boolean().
defines_behaviour(FileName) ->
  Forms = do_parse(FileName),
  case [ attribute || {attribute, _, callback, _} <- Forms] of
    [] -> false;
    _  -> true
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
  Uri = els_uri:uri(list_to_binary(Path)),
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
  Desc = io_lib:format("Issue in included file (~p): ~s", [Line, Desc0]),
  diagnostic(InclusionRange, Module, Desc, Severity).

-spec diagnostic(poi_range(), module(), string(), integer()) ->
  diagnostic().
diagnostic(Range, Module, Desc, Severity) ->
  Message = list_to_binary(lists:flatten(Module:format_error(Desc))),
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
