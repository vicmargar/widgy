-module(widgy_redis_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RedisConn = {redis_conn,
                      {redis_conn, start_link, []},
                      transient, 5000, worker, [redis_conn]},

    {ok, {{one_for_one, 5, 10}, [RedisConn]}}.