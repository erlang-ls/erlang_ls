%% @doc Simple EUnit listener that collects the results of the tests and
%%      sends them back to the parent process.

-module(els_eunit_listener).

-behaviour(eunit_listener).

-export([start/0, start/1]).
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

-record(state, {
    result = [] :: list(),
    parent_pid :: pid()
}).

-type state() :: #state{}.

-spec start() -> pid().
start() ->
    start([]).

-spec start(list()) -> pid().
start(Options) ->
    eunit_listener:start(?MODULE, Options).

-spec init(list()) -> state().
init(Options) ->
    Pid = proplists:get_value(parent_pid, Options),
    receive
        {start, _Reference} ->
            #state{parent_pid = Pid}
    end.

-spec terminate(any(), state()) -> any().
terminate({ok, _Data}, #state{parent_pid = Pid, result = Result}) ->
    Pid ! {result, Result},
    ok;
terminate({error, _Reason}, _State) ->
    sync_end(error).

-spec sync_end(any()) -> ok.
sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.

-spec handle_begin(atom(), any(), state()) -> state().
handle_begin(_Kind, _Data, State) ->
    State.

-spec handle_end(atom(), any(), state()) -> state().
handle_end(group, _Data, State) ->
    State;
handle_end(test, Data, State) ->
    State#state{result = [Data | State#state.result]}.

-spec handle_cancel(atom(), any(), state()) -> state().
handle_cancel(_Kind, _Data, State) ->
    State.
