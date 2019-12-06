-module(els_text_edit).

-export([ diff_files/2
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% File and module functions
%%==============================================================================

%% @doc Construct a [text_edit()] from the diff between two files.
%%
%% Generate a list of text edits to transform `SourceFile' into
%% `DestinationFile'
-spec diff_files(els_uri:path(), els_uri:path()) -> [text_edit()].
diff_files(SourceFile, DestinationFile) ->
    %% lager:info("diff_files: [Paths=~p]", [{SourceFile, DestinationFile}]),
    Diffs = tdiff:diff_files(SourceFile, DestinationFile),
    Patch = make_text_edits(Diffs),
    lager:info("diff_files: [Patch=~p]", [Patch]),
    Patch.


%%==============================================================================
%% Internal functions
%%==============================================================================

%% A text_edit() has a range and a blob of text.
%%
%% insert:
%%    start and end of range are the same, specifying where to include
%% delete:
%%    start and end of range define region to be removed, blank text.
%% change:
%%    start and end are original range, text is replacement text

-type diff() :: {del, [binary()]} | {eq, [binary()]} | {ins, [binary()]}.

-spec make_text_edits([diff()]) -> [text_edit()].
make_text_edits(Diffs) ->
    make_text_edits(Diffs, 0, []).

-spec make_text_edits([diff()], number(), [text_edit()]) -> [text_edit()].
make_text_edits([{eq, Data}|T], Line, Acc) ->
    make_text_edits(T, Line + length(Data), Acc);

make_text_edits([{del, Del}, {ins, Ins}|T], Line, Acc) ->
    Len = length(Del),
    Pos1 = #{ line => Line,       character => 0 },
    Pos2 = #{ line => Line + Len, character => 0 },
    Edit = #{ range => #{ start => Pos1, 'end' => Pos2 }
            , new_text => Ins
            },
    make_text_edits(T, Line + Len, [Edit|Acc]);

make_text_edits([{ins, Data}|T], Line, Acc) ->
    Pos = #{ line => Line, character => 0 },
    Edit = #{ range => #{ start => Pos, 'end' => Pos }
            , new_text => Data
            },
    make_text_edits(T, Line, [Edit|Acc]);

make_text_edits([{del, Data}|T], Line, Acc) ->
    Len = length(Data),
    Pos1 = #{ line => Line,       character => 0 },
    Pos2 = #{ line => Line + Len, character => 0 },
    Edit = #{ range => #{ start => Pos1, 'end' => Pos2 }
            , new_text => <<"">>
            },
    make_text_edits(T, Line + Len, [Edit|Acc]);

make_text_edits([], _Line, Acc) -> lists:reverse(Acc).
