%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_elvis_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ diagnostics/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================
-spec diagnostics(uri()) -> [diagnostic()].
diagnostics(Uri) ->
  RelFile = els_utils:project_relative(Uri),

  %% Note: elvis_core:rock_this requires a file path relative to the
  %%       project root, formatted as a string.
  try elvis_core:rock_this(RelFile) of
      ok -> [] ;
      {fail, Problems} -> lists:flatmap(fun format_diagnostics/1, Problems)
  catch Err ->
      lager:warning("Elvis error.[Err=~p] ", [Err]),
      []
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec format_diagnostics(any()) -> [map()].
format_diagnostics(#{file := Path, rules := Rules}) ->
  R = format_rules(Path, Rules),
  lists:flatten(R).


%%% This section is based directly on elvis_result:print_rules
-spec format_rules(any(), [any()]) -> [[map()]].
format_rules(_File, []) ->
    [];
format_rules(File, [#{items := []} | Items]) ->
    format_rules(File, Items);
format_rules(File, [#{items := Items, name := Name} | EItems]) ->
    ItemDiags = format_item(File, Name, Items),
    [lists:flatten(ItemDiags) | format_rules(File, EItems)].

%% Item
-spec format_item(any(), any(), [any()]) -> [[map()]].
format_item(File, Name,
            [#{message := Msg, line_num := Ln, info := Info} | Items]) ->
    Diagnostic = diagnostic(File, Name, Msg, Ln, Info),
    [Diagnostic | format_item(File, Name, Items)];
format_item(_File, _Name, []) ->
    [].

%%% End of section based directly on elvis_result:print_rules

-spec diagnostic(any(), any(), any(), integer(), [any()]) -> [map()].
diagnostic(_File, Name, Msg, Ln, Info) ->
  FMsg = io_lib:format(Msg, Info),
  Range   = els_protocol:range(#{ from => {Ln, 1}
                                , to   => {Ln + 1, 1}
                                }),
  Message = list_to_binary(FMsg),
  [#{ range    => Range
    , severity => ?DIAGNOSTIC_WARNING
    , code     => Name
    , source   => <<"elvis">>
    , message  => Message
    , relatedInformation => []
    }].
