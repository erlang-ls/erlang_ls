%% =============================================================================
%% An erlang_ls fork of Erlang/OTP's edoc_report
%% =============================================================================
%% The main reasons for the fork:
%%   * The edoc application does not offer an API to return a
%%     list of warnings and errors
%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc EDoc verbosity/error reporting.

-module(edoc_report).

-compile({no_auto_import, [error/1, error/2, error/3]}).
-export([error/1,
         error/2,
         error/3,
         report/2,
         report/3,
         report/4,
         warning/1,
         warning/2,
         warning/3,
         warning/4]).

-type where() :: any().
-type what() :: any().
-type line() :: non_neg_integer().
-type severity() :: warning | error.

-define(APPLICATION, edoc).
-define(DICT_KEY, edoc_diagnostics).

-spec error(what()) -> ok.
error(What) ->
  error([], What).

-spec error(where(), what()) -> ok.
error(Where, What) ->
  error(0, Where, What).

-spec error(line(), where(), any()) -> ok.
error(Line, Where, S) when is_list(S) ->
  report(Line, Where, S, [], error);
error(Line, Where, {S, D}) when is_list(S) ->
  report(Line, Where, S, D, error);
error(Line, Where, {format_error, M, D}) ->
  report(Line, Where, M:format_error(D), [], error).

-spec warning(string()) -> ok.
warning(S) ->
  warning(S, []).

-spec warning(string(), [any()]) -> ok.
warning(S, Vs) ->
  warning([], S, Vs).

-spec warning(where(), string(), [any()]) -> ok.
warning(Where, S, Vs) ->
  warning(0, Where, S, Vs).

-spec warning(line(), where(), string(), [any()]) -> ok.
warning(L, Where, S, Vs) ->
  report(L, Where, S, Vs, warning).

-spec report(string(), [any()]) -> ok.
report(S, Vs) ->
  report([], S, Vs).

-spec report(where(), string(), [any()]) -> ok.
report(Where, S, Vs) ->
  report(0, Where, S, Vs).

-spec report(line(), where(), string(), [any()]) -> ok.
report(L, Where, S, Vs) ->
  report(L, Where, S, Vs, error).

-spec report(line(), where(), string(), [any()], severity()) -> ok.
report(L, Where, S, Vs, Severity) ->
  put(?DICT_KEY, [{L, where(Where), S, Vs, Severity}|get(?DICT_KEY)]).

-spec where([any()] |
            {string(), module | footer | header | {atom(), non_neg_integer()}})
           -> string().
where({File, module}) ->
  io_lib:fwrite("~ts, in module header: ", [File]);
where({File, footer}) ->
  io_lib:fwrite("~ts, in module footer: ", [File]);
where({File, header}) ->
  io_lib:fwrite("~ts, in header file: ", [File]);
where({File, {F, A}}) ->
  io_lib:fwrite("~ts, function ~ts/~w: ", [File, F, A]);
where([]) ->
  io_lib:fwrite("~s: ", [?APPLICATION]);
where(File) when is_list(File) ->
  File ++ ": ".
