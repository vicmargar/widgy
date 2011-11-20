-module(widgy_counter).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_state/1, increment/1]).

-record(state, {count=0}).

get_state(CounterId) when is_list(CounterId) ->
    Pid = widgy:get_widget_pid(CounterId),
    gen_server:call(Pid, get_state).

increment(CounterId) when is_list(CounterId) ->
    Pid = widgy:get_widget_pid(CounterId),
    increment(Pid);

increment(Pid) ->
    gen_server:call(Pid, increment).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(get_count, _From, State) ->
    Count = State#state.count,
    {reply, Count, State};

handle_call(increment, _From, State) ->
    NewCount = State#state.count + 1,
    {reply, NewCount, State#state{count=NewCount}};

handle_call(get_state, _From, State) ->
    {reply, [{count, State#state.count}], State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
