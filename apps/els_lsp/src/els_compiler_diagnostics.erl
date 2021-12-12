%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_compiler_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-behaviour(els_diagnostics).
-export([ is_default/0
        , run/1
        , source/0
        , on_complete/2
        ]).

%% identity function for our own diagnostics
-export([ format_error/1 ]).

-export([ inclusion_range/2
        , inclusion_range/3
        ]).

-export([ include_options/0
        , macro_options/0
        , telemetry/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type compiler_info()  :: {erl_anno:anno() | 'none', module(), any()}.
-type compiler_msg()   :: {file:filename(), [compiler_info()]}.
-type macro_config()   :: #{string() => string()}.
-type macro_option()   :: {'d', atom()} | {'d', atom(), any()}.
-type include_option() :: {'i', string()}.

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
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
      ?LOG_DEBUG("Skipping diagnostics due to extension [uri=~p]", [Uri]),
      []
  end.

-spec source() -> binary().
source() ->
  <<"Compiler">>.

-spec on_complete(uri(), [els_diagnostics:diagnostic()]) -> ok.
on_complete(Uri, Diagnostics) ->
  ?MODULE:telemetry(Uri, Diagnostics),
  maybe_compile_and_load(Uri, Diagnostics).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec compile(uri()) -> [els_diagnostics:diagnostic()].
compile(Uri) ->
  Dependencies = els_diagnostics_utils:dependencies(Uri),
  Path = els_utils:to_list(els_uri:path(Uri)),
  case compile_file(Path, Dependencies) of
    {{ok, _, WS}, Diagnostics} ->
      Diagnostics ++
        diagnostics(Path, WS, ?DIAGNOSTIC_WARNING);
    {{error, ES, WS}, Diagnostics} ->
      Diagnostics ++
        diagnostics(Path, WS, ?DIAGNOSTIC_WARNING) ++
        diagnostics(Path, ES, ?DIAGNOSTIC_ERROR)
  end.

-spec parse(uri()) -> [els_diagnostics:diagnostic()].
parse(Uri) ->
  FileName = els_utils:to_list(els_uri:path(Uri)),
  Document = case els_dt_document:lookup(Uri) of
                 {ok, [DocItem]} ->
                     DocItem;
                 _ ->
                     undefined
             end,
  {ok, Epp} = epp:open([ {name, FileName}
                       , {includes, els_config:get(include_paths)}
                       ]),
  Res = [epp_diagnostic(Document, Anno, Module, Desc)
         || {error, {Anno, Module, Desc}} <- epp:parse_file(Epp)],
  epp:close(Epp),
  Res.

%% Possible cases to handle
%% ,{error,{19,erl_parse,["syntax error before: ","'-'"]}}
%% ,{error,{1,epp,{error,1,{undefined,'MODULE',none}}}}
%% ,{error,{3,epp,{error,"including nonexistent_macro.hrl is not allowed"}}}
%% ,{error,{3,epp,{include,file,"yaws.hrl"}}}
-spec epp_diagnostic(els_dt_document:item(),
                     erl_anno:anno(), module(), any()) ->
        els_diagnostics:diagnostic().
epp_diagnostic(Document, Anno, epp, {error, Anno, Reason}) ->
    %% Workaround for https://bugs.erlang.org/browse/ERL-1310
    epp_diagnostic(Document, Anno, epp, Reason);
epp_diagnostic(Document, Anno, Module, Desc) ->
    diagnostic(range(Document, Anno), Module, Desc, ?DIAGNOSTIC_ERROR).

-spec parse_escript(uri()) -> [els_diagnostics:diagnostic()].
parse_escript(Uri) ->
  FileName = els_utils:to_list(els_uri:path(Uri)),
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
-spec diagnostics(list(), [compiler_msg()], els_diagnostics:severity()) ->
        [els_diagnostics:diagnostic()].
diagnostics(Path, List, Severity) ->
  Uri = els_uri:uri(els_utils:to_binary(Path)),
  case els_utils:lookup_document(Uri) of
    {ok, Document} ->
      lists:flatten([[ diagnostic( Path
                                 , MessagePath
                                 , range(Document, Anno)
                                 , Document
                                 , Module
                                 , Desc
                                 , Severity)
                       || {Anno, Module, Desc} <- Info]
                     || {MessagePath, Info} <- List]);
    {error, _Error} ->
      []
  end.

-spec diagnostic( string()
                , string()
                , poi_range()
                , els_dt_document:item()
                , module()
                , string()
                , integer()) -> els_diagnostics:diagnostic().
diagnostic(Path, Path, Range, _Document, Module, Desc, Severity) ->
  %% The compiler message is related to the same .erl file, so
  %% preserve the location information.
  diagnostic(Range, Module, Desc, Severity);
diagnostic(_Path, MessagePath, Range, Document, Module, Desc0, Severity) ->
  #{from := {Line, _}} = Range,
  InclusionRange = inclusion_range(MessagePath, Document),
  %% The compiler message is related to an included file. Replace the
  %% original location with the location of the file inclusion.
  %% And re-route the format_error call to this module as a no-op
  Desc1 = Module:format_error(Desc0),
  Desc = io_lib:format("Issue in included file (~p): ~s", [Line, Desc1]),
  diagnostic(InclusionRange, ?MODULE, Desc, Severity).

-spec diagnostic(poi_range(), module(), string(), integer()) ->
        els_diagnostics:diagnostic().
diagnostic(Range, Module, Desc, Severity) ->
  Message0 = lists:flatten(Module:format_error(Desc)),
  Message  = els_utils:to_binary(Message0),
  Code = make_code(Module, Desc),
  #{ range    => els_protocol:range(Range)
   , message  => Message
   , severity => Severity
   , source   => source()
   , code     => Code
   }.

%% @doc NOP function for the call to 'Module:format_error/1' in diagnostic/4
%% above.
-spec format_error(string()) -> [string()].
format_error(Str) ->
  Str.


%-----------------------------------------------------------------------

%% @doc Return a unique code for each of the possible errors returned by the
%% compiler or its subsystems.

-spec make_code(atom(), any()) -> binary().

%% This file
make_code(els_compiler_diagnostics, _) ->
 <<"L0000">>;

%% compiler-8.0.2/src/compile.erl
make_code(compile, no_crypto) ->
 <<"C1000">>;
make_code(compile, bad_crypto_key) ->
 <<"C1001">>;
make_code(compile, no_crypto_key) ->
 <<"C1002">>;
make_code(compile, {open, _E}) ->
 <<"C1003">>;
make_code(compile, {epp, _E}) ->
 make_code(epp, compile);
make_code(compile, write_error) ->
 <<"C1004">>;
make_code(compile, {write_error, _Error}) ->
 <<"C1005">>;
make_code(compile, {rename, _From, _To, _Error}) ->
 <<"C1006">>;
make_code(compile, {parse_transform, _M, _R}) ->
 <<"C1007">>;
make_code(compile, {undef_parse_transform, _M}) ->
 <<"C1008">>;
make_code(compile, {core_transform, _M, _R}) ->
 <<"C1009">>;
make_code(compile, {crash, _Pass, _Reason, _Stk}) ->
 <<"C1010">>;
make_code(compile, {bad_return, _Pass, _Reason}) ->
 <<"C1011">>;
make_code(compile, {module_name, _Mod, _Filename}) ->
 <<"C1012">>;
make_code(compile, _Other) ->
 <<"C1099">>;

%% syntax_tools-2.6/src/epp_dodger.erl
make_code(epp_dodger, macro_args) ->
 <<"D1100">>;
make_code(epp_dodger, {error, _Error}) ->
 <<"D1101">>;
make_code(epp_dodger, {warning, _Error}) ->
 <<"D1102">>;
make_code(epp_dodger, {unknown, _Reason}) ->
 <<"D1103">>;
make_code(epp_dodger, _Other) ->
 <<"D1199">>;

%% stdlib-3.15.2/src/erl_lint.erl
make_code(erl_lint, undefined_module) ->
 <<"L1201">>;
make_code(erl_lint, redefine_module) ->
 <<"L1202">>;
make_code(erl_lint, pmod_unsupported) ->
 <<"L1203">>;
make_code(erl_lint, non_latin1_module_unsupported) ->
 <<"L1204">>;
make_code(erl_lint, invalid_call) ->
 <<"L1205">>;
make_code(erl_lint, invalid_record) ->
 <<"L1206">>;
make_code(erl_lint, {attribute, _A}) ->
 <<"L1207">>;
make_code(erl_lint, {missing_qlc_hrl, _A}) ->
 <<"L1208">>;
make_code(erl_lint, {redefine_import, {{_F, _A}, _M}}) ->
 <<"L1209">>;
make_code(erl_lint, {bad_inline, {_F, _A}}) ->
 <<"L1210">>;
make_code(erl_lint, {invalid_deprecated, _D}) ->
 <<"L1211">>;
make_code(erl_lint, {bad_deprecated, {_F, _A}}) ->
 <<"L1212">>;
make_code(erl_lint, {invalid_removed, _D}) ->
 <<"L1213">>;
make_code(erl_lint, {bad_removed, {F, A}}) when F =:= '_'; A =:= '_' ->
 <<"L1214">>;
make_code(erl_lint, {bad_removed, {_F, _A}}) ->
 <<"L1215">>;
make_code(erl_lint, {bad_nowarn_unused_function, {_F, _A}}) ->
 <<"L1216">>;
make_code(erl_lint, {bad_nowarn_bif_clash, {_F, _A}}) ->
 <<"L1217">>;
make_code(erl_lint, disallowed_nowarn_bif_clash) ->
 <<"L1218">>;
make_code(erl_lint, {bad_on_load, _Term}) ->
 <<"L1219">>;
make_code(erl_lint, multiple_on_loads) ->
 <<"L1220">>;
make_code(erl_lint, {bad_on_load_arity, {_F, _A}}) ->
 <<"L1221">>;
make_code(erl_lint, {undefined_on_load, {_F, _A}}) ->
 <<"L1222">>;
make_code(erl_lint, nif_inline) ->
 <<"L1223">>;
make_code(erl_lint, export_all) ->
 <<"L1224">>;
make_code(erl_lint, {duplicated_export, {_F, _A}}) ->
 <<"L1225">>;
make_code(erl_lint, {unused_import, {{_F, _A}, _M}}) ->
 <<"L1226">>;
make_code(erl_lint, {undefined_function, {_F, _A}}) ->
 <<"L1227">>;
make_code(erl_lint, {redefine_function, {_F, _A}}) ->
 <<"L1228">>;
make_code(erl_lint, {define_import, {_F, _A}}) ->
 <<"L1229">>;
make_code(erl_lint, {unused_function, {_F, _A}}) ->
 <<"L1230">>;
make_code(erl_lint, {call_to_redefined_bif, {_F, _A}}) ->
 <<"L1231">>;
make_code(erl_lint, {call_to_redefined_old_bif, {_F, _A}}) ->
 <<"L1232">>;
make_code(erl_lint, {redefine_old_bif_import, {_F, _A}}) ->
 <<"L1233">>;
make_code(erl_lint, {redefine_bif_import, {_F, _A}}) ->
 <<"L1234">>;
make_code(erl_lint, {deprecated, _MFA, _String, _Rel}) ->
 <<"L1235">>;
make_code(erl_lint, {deprecated, _MFA, String}) when is_list(String) ->
 <<"L1236">>;
make_code(erl_lint, {deprecated_type, {_M1, _F1, _A1}, _String, _Rel}) ->
 <<"L1237">>;
make_code(erl_lint, {deprecated_type, {_M1, _F1, _A1}, String})
 when is_list(String) ->
 <<"L1238">>;
make_code(erl_lint, {removed, _MFA, _ReplacementMFA, _Rel}) ->
 <<"L1239">>;
make_code(erl_lint, {removed, _MFA, String}) when is_list(String) ->
 <<"L1240">>;
make_code(erl_lint, {removed_type, _MNA, _String}) ->
 <<"L1241">>;
make_code(erl_lint, {obsolete_guard, {_F, _A}}) ->
 <<"L1242">>;
make_code(erl_lint, {obsolete_guard_overridden, _Test}) ->
 <<"L1243">>;
make_code(erl_lint, {too_many_arguments, _Arity}) ->
 <<"L1244">>;
make_code(erl_lint, illegal_pattern) ->
 <<"L1245">>;
make_code(erl_lint, illegal_map_key) ->
 <<"L1246">>;
make_code(erl_lint, illegal_bin_pattern) ->
 <<"L1247">>;
make_code(erl_lint, illegal_expr) ->
 <<"L1248">>;
make_code(erl_lint, {illegal_guard_local_call, {_F, _A}}) ->
 <<"L1249">>;
make_code(erl_lint, illegal_guard_expr) ->
 <<"L1250">>;
make_code(erl_lint, illegal_map_construction) ->
 <<"L1251">>;
make_code(erl_lint, {undefined_record, _T}) ->
 <<"L1252">>;
make_code(erl_lint, {redefine_record, _T}) ->
 <<"L1253">>;
make_code(erl_lint, {redefine_field, _T, _F}) ->
 <<"L1254">>;
make_code(erl_lint, bad_multi_field_init) ->
 <<"L1255">>;
make_code(erl_lint, {undefined_field, _T, _F}) ->
 <<"L1256">>;
make_code(erl_lint, illegal_record_info) ->
 <<"L1257">>;
make_code(erl_lint, {field_name_is_variable, _T, _F}) ->
 <<"L1258">>;
make_code(erl_lint, {wildcard_in_update, _T}) ->
 <<"L1259">>;
make_code(erl_lint, {unused_record, _T}) ->
 <<"L1260">>;
make_code(erl_lint, {untyped_record, _T}) ->
 <<"L1261">>;
make_code(erl_lint, {unbound_var, _V}) ->
 <<"L1262">>;
make_code(erl_lint, {unsafe_var, _V, {_What, _Where}}) ->
 <<"L1263">>;
make_code(erl_lint, {exported_var, _V, {_What, _Where}}) ->
 <<"L1264">>;
make_code(erl_lint, {match_underscore_var, _V}) ->
 <<"L1265">>;
make_code(erl_lint, {match_underscore_var_pat, _V}) ->
 <<"L1266">>;
make_code(erl_lint, {shadowed_var, _V, _In}) ->
 <<"L1267">>;
make_code(erl_lint, {unused_var, _V}) ->
 <<"L1268">>;
make_code(erl_lint, {variable_in_record_def, _V}) ->
 <<"L1269">>;
make_code(erl_lint, {stacktrace_guard, _V}) ->
 <<"L1270">>;
make_code(erl_lint, {stacktrace_bound, _V}) ->
 <<"L1271">>;
make_code(erl_lint, {undefined_bittype, _Type}) ->
 <<"L1272">>;
make_code(erl_lint, {bittype_mismatch, _Val1, _Val2, _What}) ->
 <<"L1273">>;
make_code(erl_lint, bittype_unit) ->
 <<"L1274">>;
make_code(erl_lint, illegal_bitsize) ->
 <<"L1275">>;
make_code(erl_lint, {illegal_bitsize_local_call, {_F, _A}}) ->
 <<"L1276">>;
make_code(erl_lint, non_integer_bitsize) ->
 <<"L1277">>;
make_code(erl_lint, unsized_binary_not_at_end) ->
 <<"L1278">>;
make_code(erl_lint, typed_literal_string) ->
 <<"L1279">>;
make_code(erl_lint, utf_bittype_size_or_unit) ->
 <<"L1280">>;
make_code(erl_lint, {bad_bitsize, _Type}) ->
 <<"L1281">>;
make_code(erl_lint, unsized_binary_in_bin_gen_pattern) ->
 <<"L1282">>;
make_code(erl_lint, {conflicting_behaviours,
 {_Name, _Arity}, _B, _FirstL, _FirstB}) ->
 <<"L1283">>;
make_code(erl_lint, {undefined_behaviour_func, {_Func, _Arity}, _Behaviour}) ->
 <<"L1284">>;
make_code(erl_lint, {undefined_behaviour, _Behaviour}) ->
 <<"L1285">>;
make_code(erl_lint, {undefined_behaviour_callbacks, _Behaviour}) ->
 <<"L1286">>;
make_code(erl_lint, {ill_defined_behaviour_callbacks, _Behaviour}) ->
 <<"L1287">>;
make_code(erl_lint, {ill_defined_optional_callbacks, _Behaviour}) ->
 <<"L1288">>;
make_code(erl_lint, {behaviour_info, {_M, _F, _A}}) ->
 <<"L1289">>;
make_code(erl_lint, {redefine_optional_callback, {_F, _A}}) ->
 <<"L1290">>;
make_code(erl_lint, {undefined_callback, {_M, _F, _A}}) ->
 <<"L1291">>;
make_code(erl_lint, {singleton_typevar, _Name}) ->
 <<"L1292">>;
make_code(erl_lint, {bad_export_type, _ETs}) ->
 <<"L1293">>;
make_code(erl_lint, {duplicated_export_type, {_T, _A}}) ->
 <<"L1294">>;
make_code(erl_lint, {undefined_type, {_TypeName, _Arity}}) ->
 <<"L1295">>;
make_code(erl_lint, {unused_type, {_TypeName, _Arity}}) ->
 <<"L1296">>;
make_code(erl_lint, {new_builtin_type, {_TypeName, _Arity}}) ->
 <<"L1297">>;
make_code(erl_lint, {builtin_type, {_TypeName, _Arity}}) ->
 <<"L1298">>;
make_code(erl_lint, {renamed_type, _OldName, _NewName}) ->
 <<"L1299">>;
make_code(erl_lint, {redefine_type, {_TypeName, _Arity}}) ->
 <<"L1300">>;
make_code(erl_lint, {type_syntax, _Constr}) ->
 <<"L1301">>;
make_code(erl_lint, old_abstract_code) ->
 <<"L1302">>;
make_code(erl_lint, {redefine_spec, {_M, _F, _A}}) ->
 <<"L1303">>;
make_code(erl_lint, {redefine_spec, {_F, _A}}) ->
 <<"L1304">>;
make_code(erl_lint, {redefine_callback, {_F, _A}}) ->
 <<"L1305">>;
make_code(erl_lint, {bad_callback, {_M, _F, _A}}) ->
 <<"L1306">>;
make_code(erl_lint, {bad_module, {_M, _F, _A}}) ->
 <<"L1307">>;
make_code(erl_lint, {spec_fun_undefined, {_F, _A}}) ->
 <<"L1308">>;
make_code(erl_lint, {missing_spec, {_F, _A}}) ->
 <<"L1309">>;
make_code(erl_lint, spec_wrong_arity) ->
 <<"L1310">>;
make_code(erl_lint, callback_wrong_arity) ->
 <<"L1311">>;
make_code(erl_lint, {deprecated_builtin_type, {_Name, _Arity},
              _Replacement, _Rel}) ->
 <<"L1312">>;
make_code(erl_lint, {not_exported_opaque, {_TypeName, _Arity}}) ->
 <<"L1313">>;
make_code(erl_lint, {underspecified_opaque, {_TypeName, _Arity}}) ->
 <<"L1314">>;
make_code(erl_lint, {bad_dialyzer_attribute, _Term}) ->
 <<"L1315">>;
make_code(erl_lint, {bad_dialyzer_option, _Term}) ->
 <<"L1316">>;
make_code(erl_lint, {format_error, {_Fmt, _Args}}) ->
 <<"L1317">>;
make_code(erl_lint, _Other) ->
 <<"L1399">>;

%% stdlib-3.15.2/src/erl_scan.erl
make_code(erl_scan, {string, _Quote, _Head}) ->
 <<"S1400">>;
make_code(erl_scan, {illegal, _Type}) ->
 <<"S1401">>;
make_code(erl_scan, char) ->
 <<"S1402">>;
make_code(erl_scan, {base, _Base}) ->
 <<"S1403">>;
make_code(erl_scan,  _Other) ->
 <<"S1499">>;

%% stdlib-3.15.2/src/epp.erl
make_code(epp, cannot_parse) ->
 <<"E1500">>;
make_code(epp, {bad, _W}) ->
 <<"E1501">>;
make_code(epp, {duplicated_argument, _Arg}) ->
 <<"E1502">>;
make_code(epp, missing_parenthesis) ->
 <<"E1503">>;
make_code(epp, missing_comma) ->
 <<"E1504">>;
make_code(epp, premature_end) ->
 <<"E1505">>;
make_code(epp, {call, _What}) ->
 <<"E1506">>;
make_code(epp, {undefined, _M, none}) ->
 <<"E1507">>;
make_code(epp, {undefined, _M, _A}) ->
 <<"E1508">>;
make_code(epp, {depth, _What}) ->
 <<"E1509">>;
make_code(epp, {mismatch, _M}) ->
 <<"E1510">>;
make_code(epp, {arg_error, _M}) ->
 <<"E1511">>;
make_code(epp, {redefine, _M}) ->
 <<"E1512">>;
make_code(epp, {redefine_predef, _M}) ->
 <<"E1513">>;
make_code(epp, {circular, _M, none}) ->
 <<"E1514">>;
make_code(epp, {circular, _M, _A}) ->
 <<"E1515">>;
make_code(epp, {include, _W, _F}) ->
 <<"E1516">>;
make_code(epp, {illegal, _How, _What}) ->
 <<"E1517">>;
make_code(epp, {illegal_function, _Macro}) ->
 <<"E1518">>;
make_code(epp, {illegal_function_usage, _Macro}) ->
 <<"E1519">>;
make_code(epp, elif_after_else) ->
 <<"E1520">>;
make_code(epp, {'NYI', _What}) ->
 <<"E1521">>;
make_code(epp, {error, _Term}) ->
 <<"E1522">>;
make_code(epp, {warning, _Term}) ->
 <<"E1523">>;
make_code(epp, _E) ->
 <<"E1599">>;

%% stdlib-3.15.2/src/qlc.erl
make_code(qlc, not_a_query_list_comprehension) ->
 <<"Q1600">>;
make_code(qlc, {used_generator_variable, _V}) ->
 <<"Q1601">>;
make_code(qlc, binary_generator) ->
 <<"Q1602">>;
make_code(qlc, too_complex_join) ->
 <<"Q1603">>;
make_code(qlc, too_many_joins) ->
 <<"Q1604">>;
make_code(qlc, nomatch_pattern) ->
 <<"Q1605">>;
make_code(qlc, nomatch_filter) ->
 <<"Q1606">>;
make_code(qlc, {Location, _Mod, _Reason}) when is_integer(Location) ->
 <<"Q1607">>;
make_code(qlc, {bad_object, _FileName}) ->
 <<"Q1608">>;
make_code(qlc, bad_object) ->
 <<"Q1609">>;
make_code(qlc, {file_error, _FileName, _Reason}) ->
 <<"Q1610">>;
make_code(qlc, {premature_eof, _FileName}) ->
 <<"Q1611">>;
make_code(qlc, {tmpdir_usage, _Why}) ->
 <<"Q1612">>;
make_code(qlc, {error, _Module, _Reason}) ->
 <<"Q1613">>;
make_code(qlc, _E) ->
 <<"Q1699">>;

%% stdlib-3.15.2/src/erl_parse.yrl
make_code(erl_parse, "head mismatch") ->
 <<"P1700">>;
make_code(erl_parse, "bad type variable") ->
 <<"P1701">>;
make_code(erl_parse, "bad attribute") ->
 <<"P1702">>;
make_code(erl_parse, "unsupported constraint" ++ _) ->
 <<"P1703">>;
make_code(erl_parse, "bad binary type") ->
 <<"P1704">>;
make_code(erl_parse, "bad variable list") ->
 <<"P1705">>;
make_code(erl_parse, "bad function arity") ->
 <<"P1706">>;
make_code(erl_parse, "bad function name") ->
 <<"P1707">>;
make_code(erl_parse, "bad Name/Arity") ->
 <<"P1708">>;
make_code(erl_parse, "bad record declaration") ->
 <<"P1709">>;
make_code(erl_parse, "bad record field") ->
 <<"P1710">>;
make_code(erl_parse, ["syntax error before: ", _]) ->
 <<"P1711">>;
%% Matching 'io_lib:format("bad ~tw declaration", [S])).', must come last
make_code(erl_parse, "bad " ++ _Str) ->
 <<"P1798">>;
make_code(erl_parse, _Other) ->
 <<"P1799">>;

make_code(Module, _Reason) ->
  unicode:characters_to_binary(io_lib:format("~p", [Module])).

%-----------------------------------------------------------------------

-spec range(els_dt_document:item() | undefined,
            erl_anno:anno() | none) -> poi_range().
range(Document, Anno) ->
  els_diagnostics_utils:range(Document, Anno).

%% @doc Find the inclusion range for a header file.
%%
%%      Given the path of e .hrl path, find its inclusion range within
%%      a given document.
-spec inclusion_range(string(), els_dt_document:item()) -> poi_range().
inclusion_range(IncludePath, Document) ->
  case
    inclusion_range(IncludePath, Document, include) ++
    inclusion_range(IncludePath, Document, include_lib) ++
    inclusion_range(IncludePath, Document, behaviour) ++
    inclusion_range(IncludePath, Document, parse_transform)
  of
    [Range|_] -> Range;
    _ -> range(undefined, none)
  end.

-spec inclusion_range( string()
                     , els_dt_document:item()
                     , include | include_lib | behaviour | parse_transform)
                     -> [poi_range()].
inclusion_range(IncludePath, Document, include) ->
  POIs       = els_dt_document:pois(Document, [include]),
  IncludeId  = els_utils:include_id(IncludePath),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= IncludeId];
inclusion_range(IncludePath, Document, include_lib) ->
  POIs       = els_dt_document:pois(Document, [include_lib]),
  IncludeId  = els_utils:include_lib_id(IncludePath),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= IncludeId];
inclusion_range(IncludePath, Document, behaviour) ->
  POIs        = els_dt_document:pois(Document, [behaviour]),
  BehaviourId = els_uri:module(els_uri:uri(els_utils:to_binary(IncludePath))),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= BehaviourId];
inclusion_range(IncludePath, Document, parse_transform) ->
  POIs       = els_dt_document:pois(Document, [parse_transform]),
  ParseTransformId
    = els_uri:module(els_uri:uri(els_utils:to_binary(IncludePath))),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= ParseTransformId].

-spec macro_options() -> [macro_option()].
macro_options() ->
  Macros = els_config:get(macros),
  [macro_option(M) || M <- Macros].

-spec macro_option(macro_config()) -> macro_option().
macro_option(#{"name" := Name, "value" := Value}) ->
  {'d', list_to_atom(Name), els_utils:macro_string_to_term(Value)};
macro_option(#{"name" := Name}) ->
  {'d', list_to_atom(Name), true}.

-spec include_options() -> [include_option()].
include_options() ->
  Paths = els_config:get(include_paths),
  [ {i, Path} || Path <- Paths ].

-spec diagnostics_options() -> [any()].
diagnostics_options() ->
  [basic_validation|diagnostics_options_bare()].

-spec diagnostics_options_load_code() -> [any()].
diagnostics_options_load_code() ->
  [binary|diagnostics_options_bare()].

-spec diagnostics_options_bare() -> [any()].
diagnostics_options_bare() ->
  lists:append([ macro_options()
               , include_options()
               , [ return_warnings
                 , return_errors
                 ]]).

-spec compile_file(string(), [atom()]) ->
        {{ok | error, [compiler_msg()], [compiler_msg()]}
        , [els_diagnostics:diagnostic()]}.
compile_file(Path, Dependencies) ->
  %% Load dependencies required for the compilation
  Olds = [load_dependency(Dependency, Path)
          || Dependency <- Dependencies
               , not code:is_sticky(Dependency) ],
  Res = compile:file(Path, diagnostics_options()),
  %% Restore things after compilation
  [code:load_binary(Dependency, Filename, Binary)
   || {{Dependency, Binary, Filename}, _} <- Olds],
  Diagnostics = lists:flatten([ Diags || {_, Diags} <- Olds ]),
  {Res, Diagnostics ++ module_name_check(Path)}.

%% The module_name error is only emitted by the Erlang compiler during
%% the "save binary" phase. This phase does not occur when code
%% generation is disabled (e.g. by using the basic_validation or
%% strong_validation arguments when invoking the compile:file/2
%% function), which is exactly the case for the Erlang LS compiler
%% diagnostics. Therefore, let's replicate the check explicitly.
%% See issue #1152.
-spec module_name_check(string()) -> [els_diagnostics:diagnostic()].
module_name_check(Path) ->
  Basename = filename:basename(Path, ".erl"),
  Uri = els_uri:uri(els_utils:to_binary(Path)),
  case els_dt_document:lookup(Uri) of
    {ok, [Document]} ->
      case els_dt_document:pois(Document, [module]) of
        [#{id := Module, range := Range}] ->
          case atom_to_list(Module) =:= Basename of
            true ->
              [];
            false ->
              Message =
                io_lib:format("Module name '~s' does not match file name '~ts'",
                              [Module, Basename]),
              Diagnostic = els_diagnostics:make_diagnostic(
                             els_protocol:range(Range),
                             els_utils:to_binary(Message),
                             ?DIAGNOSTIC_ERROR,
                             <<"Compiler (via Erlang LS)">>),
              [Diagnostic]
          end
      end;
    _ ->
      []
  end.

%% @doc Load a dependency, return the old version of the code (if any),
%% so it can be restored.
-spec load_dependency(atom(), string()) ->
        {{atom(), binary(), file:filename()}, [els_diagnostics:diagnostic()]}
          | error.
load_dependency(Module, IncludingPath) ->
  Old = code:get_object_code(Module),
  Diagnostics =
    case els_utils:find_module(Module) of
      {ok, Uri} ->
        Path = els_utils:to_list(els_uri:path(Uri)),
        Opts = compile_options(Module),
        case compile:file(Path, diagnostics_options_load_code() ++ Opts) of
          {ok, [], []} ->
            [];
          {ok, Module, Binary} ->
            code:load_binary(Module, atom_to_list(Module), Binary),
            [];
          {ok, Module, Binary, WS} ->
            code:load_binary(Module, atom_to_list(Module), Binary),
            diagnostics(IncludingPath, WS, ?DIAGNOSTIC_WARNING);
          {error, ES, WS} ->
            diagnostics(IncludingPath, WS, ?DIAGNOSTIC_WARNING) ++
              diagnostics(IncludingPath, ES, ?DIAGNOSTIC_ERROR)
        end;
      {error, Error} ->
        ?LOG_WARNING( "Error finding dependency [module=~p] [error=~p]"
                    , [Module, Error]),
        []
    end,
  {Old, Diagnostics}.

-spec maybe_compile_and_load(uri(), [els_diagnostics:diagnostic()]) -> ok.
maybe_compile_and_load(Uri, [] = _CDiagnostics) ->
  case els_config:get(code_reload) of
    #{"node" := NodeStr} ->
      Node = els_utils:compose_node_name(NodeStr,
                                         els_config_runtime:get_name_type()),
      Module = els_uri:module(Uri),
      case rpc:call(Node, code, is_sticky, [Module]) of
        true -> ok;
        _ -> handle_rpc_result(rpc:call(Node, c, c, [Module]), Module)
      end;
    disabled ->
      ok
  end;
maybe_compile_and_load(_Uri, _CDiagnostics) ->
  ok.

-spec handle_rpc_result(term() | {badrpc, term()}, atom()) -> ok.
handle_rpc_result({ok, Module}, _) ->
  Msg = io_lib:format("code_reload success for: ~s", [Module]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => els_utils:to_binary(Msg)
                                });
handle_rpc_result(Err, Module) ->
  ?LOG_INFO("[code_reload] code_reload using c:c/1 crashed with: ~p",
            [Err]),
  Msg = io_lib:format("code_reload swap crashed for: ~s with: ~w",
                      [Module, Err]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_ERROR,
                                  message => els_utils:to_binary(Msg)
                                }).

%% @doc Return the compile options from the compile_info chunk
-spec compile_options(atom()) -> [any()].
compile_options(Module) ->
  case code:which(Module) of
    non_existing ->
      ?LOG_DEBUG("Could not find compile options. [module=~p]", [Module]),
      [];
    Beam ->
      case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {_, Chunks}} ->
          Info = proplists:get_value(compile_info, Chunks),
          proplists:get_value(options, Info, []);
        Error ->
          ?LOG_DEBUG( "Error extracting compile_info. [module=~p] [error=~p]"
                    , [Module, Error]),
          []
      end
  end.

%% @doc Send a telemetry/event LSP message, for logging in the client
-spec telemetry(uri(), [els_diagnostics:diagnostic()]) -> ok.
telemetry(Uri, Diagnostics) ->
  case els_config:get(compiler_telemetry_enabled) of
    true ->
      Codes = [Code || #{ code := Code } <- Diagnostics ],
      Method = <<"telemetry/event">>,
      Params = #{ uri         => Uri
                , diagnostics => Codes
                , type => <<"erlang-diagnostic-codes">>
                },
      els_server:send_notification(Method, Params);
    _ ->
      ok
  end.
