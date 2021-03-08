%%%=============================================================================
%%% @doc The snippets gen_server.
%%% @end
%%%=============================================================================
-module(els_snippets_server).

%%==============================================================================
%% API
%%==============================================================================
-export([ builtin_snippets_dir/0
        , custom_snippets_dir/0
        , snippets/0
        , start_link/0
        ]).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).
-type state() :: #{}.

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TABLE, snippets).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type snippet() :: {string(), binary()}.

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, unused, []).

-spec snippets() -> [completion_item()].
snippets() ->
  [build_snippet(Entry) || Entry <- ets:tab2list(?TABLE)].

-spec builtin_snippets_dir() -> file:filename_all().
builtin_snippets_dir() ->
  filename:join(code:priv_dir(?APP), "snippets").

-spec custom_snippets_dir() -> file:filename_all().
custom_snippets_dir() ->
  {ok, [[Home]]} = init:get_argument(home),
  filename:join([Home, ".config", "erlang_ls", "snippets"]).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-spec init(unused) -> {ok, state()}.
init(unused) ->
  load_snippets(),
  {ok, #{}}.

-spec handle_call(any(), {pid(), any()}, state()) -> {reply, any(), state()}.
handle_call(_Request, _From, State) ->
  {reply, not_implemented, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Request, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec load_snippets() -> ok.
load_snippets() ->
  init_snippets_table(),
  [insert_snippet(S) || S <- builtin_snippets() ++ custom_snippets()],
  ok.

-spec init_snippets_table() -> ok.
init_snippets_table() ->
  ?TABLE = ets:new(?TABLE, [public, named_table, {read_concurrency, true}]),
  ok.

-spec insert_snippet(snippet()) -> ok.
insert_snippet({Name, Content}) ->
  true = ets:insert(?TABLE, {unicode:characters_to_binary(Name), Content}),
  ok.

-spec builtin_snippets() -> [snippet()].
builtin_snippets() ->
  case filelib:is_dir(code:priv_dir(?APP)) of
    false ->
      %% Probably running within an escript, so we need to extract the
      %% snippets from the archive
      snippets_from_escript();
    true ->
      snippets_from_dir(builtin_snippets_dir())
  end.

-spec snippets_from_escript() -> [snippet()].
snippets_from_escript() ->
  Name = escript:script_name(),
  {ok, Sections} = escript:extract(Name, []),
  Archive = proplists:get_value(archive, Sections),
  Fun = fun("els_lsp/priv/snippets/" ++ N, _GetInfo, GetBin, Acc) ->
            [{N, GetBin()}|Acc];
           (_Name, _GetInfo, _GetBin, Acc) ->
            Acc
        end,
  {ok, Snippets} = zip:foldl(Fun, [], {Name, Archive}),
  Snippets.

-spec custom_snippets() -> [snippet()].
custom_snippets() ->
  Dir = custom_snippets_dir(),
  ensure_dir(Dir),
  snippets_from_dir(Dir).

-spec snippets_from_dir(file:filename_all()) -> [snippet()].
snippets_from_dir(Dir) ->
  [snippet_from_file(Dir, Filename) || Filename <- filelib:wildcard("*", Dir)].

-spec snippet_from_file(file:filename_all(), file:filename_all()) -> snippet().
snippet_from_file(Dir, Filename) ->
  {ok, Content} = file:read_file(filename:join(Dir, Filename)),
  {Filename, Content}.

-spec ensure_dir(file:filename_all()) -> ok.
ensure_dir(Dir) ->
  ok = filelib:ensure_dir(filename:join(Dir, "dummy")).

-spec build_snippet({binary(), binary()}) -> completion_item().
build_snippet({Name, Snippet}) ->
  #{ label => <<"snippet_", Name/binary>>
   , kind => ?COMPLETION_ITEM_KIND_SNIPPET
   , insertText => Snippet
   , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
   }.
