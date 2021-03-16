%%==============================================================================
%% Code Lens: suggest_spec
%%==============================================================================
-module(els_code_lens_suggest_spec).

-behaviour(els_code_lens).
-export([ init/1
        , command/3
        , is_default/0
        , pois/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #{'info' => els_typer:info() | 'no_info'}.

%%==============================================================================
%% Callback functions for the els_code_lens behaviour
%%==============================================================================
-spec init(els_dt_document:item()) -> state().
init(#{uri := Uri} = _Document) ->
  try els_typer:get_info(Uri) of
    Info ->
      #{info => Info}
  catch C:E:S ->
      Fmt =
        "Cannot extract typer info.~n"
        "Class: ~p~n"
        "Exception: ~p~n"
        "Stacktrace: ~p~n",
      ?LOG_WARNING(Fmt, [C, E, S]),
      #{info => 'no_info'}
  end.

-spec command(els_dt_document:item(), poi(), state()) -> els_command:command().
command(Document, #{range := #{from := {Line, _}}} = POI, #{info := Info}) ->
  #{uri := Uri} = Document,
  Spec = get_type_spec(POI, Info),
  Title = Spec,
  CommandId = <<"suggest-spec">>,
  CommandArgs = [ #{ uri => Uri
                   , line => Line
                   , spec => Spec
                   }
                ],
  els_command:make_command(Title, CommandId, CommandArgs).

-spec is_default() -> boolean().
is_default() ->
  true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  Functions = els_dt_document:pois(Document, [function]),
  Specs = els_dt_document:pois(Document, [spec]),
  SpecsIds = [Id || #{id := Id} <- Specs],
  [POI || #{id := Id} = POI <- Functions, not lists:member(Id, SpecsIds)].

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec get_type_spec(poi(), els_typer:info() | 'no_info') -> binary().
get_type_spec(POI, Info) ->
  case Info of
    'no_info' ->
      <<"Cannot extract specs (check logs for details)">>;
    _ ->
      #{id := {Function, Arity}} = POI,
      Spec = els_typer:get_type_spec(Function, Arity, Info),
      re:replace(Spec, ",", ", ", [global, {return, binary}])
  end.
