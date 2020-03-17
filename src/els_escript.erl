%% =============================================================================
%% An erlang_ls stripped-down version of Erlang/OTP's escript.erl
%% =============================================================================
%% The main reasons for the fork:
%%   * The escript module was printing out to stdout warnings/errors, instead of
%%     returning them
%% =============================================================================
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(els_escript).

-export([ extract/1 ]).

-record(state, {file         :: file:filename(),
                module       :: module(),
                forms_or_bin,
                exports_main :: boolean()}).
-type state() :: #state{}.

-type shebang() :: string().

-record(sections, {shebang :: shebang() | 'undefined', body}).
-type sections() :: #sections{}.

-spec extract(file:filename()) -> any().
extract(File) ->
  {HeaderSz, NextLineNo, Fd, _Sections} = parse_header(File),
  case compile_source(File, Fd, NextLineNo, HeaderSz) of
    {ok, _Bin, Warnings} ->
      {ok, Warnings};
    {error, Errors, Warnings} ->
      {error, Errors, Warnings}
  end.

-spec compile_source( file:filename()
                    , any()
                    , pos_integer()
                    , pos_integer()) ->
        any().
compile_source(File, Fd, NextLineNo, HeaderSz) ->
  Forms = do_parse_file(File, Fd, NextLineNo, HeaderSz),
  ok = file:close(Fd),
  case compile:forms(Forms, [return_warnings, return_errors, debug_info]) of
    {ok, _, BeamBin, Warnings} ->
      {ok, BeamBin, Warnings};
    {error, Errors, Warnings} ->
      {error, Errors, Warnings}
  end.

-spec do_parse_file(any(), any(), pos_integer(), any()) ->
        [any()].
do_parse_file(File, Fd, NextLineNo, HeaderSz) ->
  S = initial_state(File),
  #state{forms_or_bin = FormsOrBin} =
    parse_source(S, File, Fd, NextLineNo, HeaderSz),
  FormsOrBin.

-spec initial_state(_) -> state().
initial_state(File) ->
  #state{file = File,
         exports_main = false}.

%% Skip header and make a heuristic guess about the script type
-spec parse_header(file:filename()) -> {any(), any(), any(), sections()}.
parse_header(File) ->
  {ok, Fd} = file:open(File, [read]),

  %% Skip shebang on first line
  Line1 = get_line(Fd),
  case classify_line(Line1) of
    shebang ->
      find_first_body_line(Fd, #sections{shebang = Line1});
    _ ->
      find_first_body_line(Fd, #sections{})
  end.

-spec find_first_body_line(_, sections()) -> {any(), any(), any(), sections()}.
find_first_body_line(Fd, Sections) ->
  {ok, HeaderSz1} = file:position(Fd, cur),
  %% Look for special comment on second line
  Line2 = get_line(Fd),
  {ok, HeaderSz2} = file:position(Fd, cur),
  case classify_line(Line2) of
    emu_args ->
      %% Skip special comment on second line
      {HeaderSz2, 3, Fd, Sections};
    comment ->
      %% Look for special comment on third line
      Line3 = get_line(Fd),
      {ok, HeaderSz3} = file:position(Fd, cur),
      Line3Type = classify_line(Line3),
      case Line3Type of
        emu_args ->
          %% Skip special comment on third line
          {HeaderSz3, 4, Fd, Sections};
        _ ->
          %% Skip shebang on first line and comment on second
          {HeaderSz2, 3, Fd, Sections}
      end;
    _ ->
      %% Just skip shebang on first line
      {HeaderSz1, 2, Fd,
       Sections#sections{}}
  end.

-spec classify_line(_) -> atom().
classify_line(Line) ->
  case Line of
    "#!" ++ _ -> shebang;
    "PK" ++ _ -> archive;
    "FOR1" ++ _ -> beam;
    "%%!" ++ _ -> emu_args;
    "%" ++ _ -> comment;
    _ -> undefined
  end.

-spec get_line(_) -> any().
get_line(P) ->
  case io:get_line(P, '') of
    eof ->
      throw("Premature end of file reached");
    Line ->
      Line
  end.

-spec parse_source(state(), _, _, pos_integer(), _) -> state().
parse_source(S, File, Fd, StartLine, HeaderSz) ->
  {PreDefMacros, DefModule} = pre_def_macros(File),
  IncludePath = [],
  %% Read the encoding on the second line, if there is any:
  {ok, _} = file:position(Fd, 0),
  _ = io:get_line(Fd, ''),
  Encoding = epp:set_encoding(Fd),
  {ok, _} = file:position(Fd, HeaderSz),
  {ok, Epp} = epp:open(File, Fd, StartLine, IncludePath, PreDefMacros),
  _ = [io:setopts(Fd, [{encoding, Encoding}]) || Encoding =/= none],
  {ok, FileForm} = epp:parse_erl_form(Epp),
  OptModRes = epp:parse_erl_form(Epp),
  S2 =
    case OptModRes of
      {ok, {attribute, _, module, M} = Form} ->
        epp_parse_file(Epp, S#state{module = M}, [Form, FileForm]);
      {ok, _} ->
        ModForm = {attribute, erl_anno:new(1), module, DefModule},
        epp_parse_file2(Epp, S#state{module = DefModule}, [ModForm, FileForm],
                        OptModRes);
      {error, _} ->
        epp_parse_file2(Epp, S#state{module = DefModule}, [FileForm],
                        OptModRes);
      {eof, LastLine} ->
        S#state{forms_or_bin = [FileForm, {eof, LastLine}]}
    end,
  ok = epp:close(Epp),
  ok = file:close(Fd),
  check_source(S2).

-spec check_source(state()) -> state().
check_source(S) ->
  case S of
    #state{exports_main = ExpMain,
           forms_or_bin = [FileForm2, ModForm2 | Forms]} ->
      %% Optionally add export of main/1
      Forms2 =
        case ExpMain of
          false -> [{attribute, erl_anno:new(0), export, [{main, 1}]} | Forms];
          true  -> Forms
        end,
      Forms3 = [FileForm2, ModForm2 | Forms2],
      S#state{forms_or_bin = Forms3}
  end.

-spec pre_def_macros(_) -> {any(), any()}.
pre_def_macros(File) ->
  {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
  Unique = erlang:unique_integer([positive]),
  Replace = fun(Char) ->
                case Char of
                  $\. -> $\_;
                  _ -> Char
                end
            end,
  CleanBase = lists:map(Replace, filename:basename(File)),
  ModuleStr =
    CleanBase ++ "__" ++
    "escript__" ++
    integer_to_list(MegaSecs) ++ "__" ++
    integer_to_list(Secs) ++ "__" ++
    integer_to_list(MicroSecs) ++ "__" ++
    integer_to_list(Unique),
  Module = list_to_atom(ModuleStr),
  PreDefMacros = [{'MODULE', Module, redefine},
                  {'MODULE_STRING', ModuleStr, redefine}],
  {PreDefMacros, Module}.

-spec epp_parse_file(_, state(), [any()]) -> state().
epp_parse_file(Epp, S, Forms) ->
  Parsed = epp:parse_erl_form(Epp),
  epp_parse_file2(Epp, S, Forms, Parsed).

-spec epp_parse_file2(_, state(), [any()], any()) -> state().
epp_parse_file2(Epp, S, Forms, Parsed) ->
  case Parsed of
    {ok, {attribute, Ln, mode, Mode} = Form} ->
      case is_valid(Mode) of
        true ->
          epp_parse_file(Epp, S, [Form | Forms]);
        false ->
          Args = lists:flatten(
                   io_lib:format("illegal mode attribute: ~p", [Mode])),
          Error = {error, {Ln, erl_parse, Args}},
          epp_parse_file(Epp, S, [Error | Forms])
      end;
    {ok, {attribute, _, export, Fs} = Form} ->
      case lists:member({main, 1}, Fs) of
        false ->
          epp_parse_file(Epp, S, [Form | Forms]);
        true ->
          epp_parse_file(Epp, S#state{exports_main = true}, [Form | Forms])
      end;
    {ok, Form} ->
      epp_parse_file(Epp, S, [Form | Forms]);
    {error, _} = Form ->
      epp_parse_file(Epp, S, [Form | Forms]);
    {eof, LastLine} ->
      S#state{forms_or_bin = lists:reverse([{eof, LastLine} | Forms])}
  end.

-spec is_valid(atom()) -> boolean().
is_valid(Mode) ->
  lists:member(Mode, [compile, debug, interpret, native]).
