-module(widgy_counter_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_counter/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-include_lib("widgy/include/widgy.hrl").

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ets:new(?COUNTERS_TABLE, [named_table, public, set]),

    WidgyCounter = {widgy_counter,
                    {widgy_counter, start_link, []},
                    transient, 5000, worker, [widgy_counter]},

    {ok, {{simple_one_for_one, 5, 10}, [WidgyCounter]}}.

start_counter() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    Guid = uuid:new(),
    ets:insert(?COUNTERS_TABLE, {Guid, Pid}),
    {Guid, Pid}.

delete_counter(Guid) ->
    [{Guid, Pid}] = ets:lookup(?COUNTERS_TABLE, Guid),
    ets:delete(?COUNTERS_TABLE, Guid),
    gen_server:call(Pid, stop).
