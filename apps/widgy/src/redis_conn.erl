-module(redis_conn).

-behaviour(gen_server).

-record(state, {connection}).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get/1, set/2]).

get(Key) ->
  q(["GET", Key]).

set(Key, Value) ->
  q(["SET", Key, Value]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Connection} = eredis:start_link(),
    {ok, #state{connection=Connection}}.

handle_call({q, Args}, _From, #state{connection=Connection}=State) ->
    Reply = eredis:q(Connection, Args),
    {reply, Reply, State};

handle_call(Request, _From, #state{connection=Connection}=State) ->
    io:format("Got: ~p ~p~n", [Request, Connection]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

q(Args) ->
    gen_server:call(?MODULE, {q, Args}).