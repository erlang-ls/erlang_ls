-module(els_diagnostics_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0
        , on_save/1
        , on_open/1
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec on_save(uri()) -> ok.
on_save(Uri) ->
  Id = binary_to_atom(Uri, utf8),
  case supervisor:get_childspec(?SERVER, Id) of
    {ok, _} ->
      ok = supervisor:terminate_child(?SERVER, Id),
      start_server(Id, Uri);
    _ ->
      start_server(Id, Uri)
  end.

-spec on_open(uri()) -> ok.
on_open(Uri) ->
  Id = binary_to_atom(Uri, utf8),
  case supervisor:get_childspec(?SERVER, Id) of
    {ok, _} -> ok;
    _ -> start_server(Id, Uri)
  end.

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => one_for_one
              , intensity => 5
              , period    => 1
              },
  {ok, {SupFlags, []}}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec start_server(atom(), uri()) -> ok.
start_server(Id, Uri) ->
  {ok, _Pid} = supervisor:start_child(?SERVER, child_spec({Id, Uri})),
  ok.

-spec child_spec({atom(), uri()}) -> map().
child_spec({Id, Uri}) ->
  #{ id       => Id
   , start    => {els_diagnostics_server, start_link, [{Id, Uri}, []]}
   , restart => temporary
   , shutdown => brutal_kill
   , type => worker
   , modules => [els_diagnostics_server]
   }.
