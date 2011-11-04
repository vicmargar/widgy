
-module(widgy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Port} = application:get_env(widgy, websockets_port),
    WidgyWebsocketsServer = {widgy_websockets_server,
                             {widgy_websockets_server, start_link, [Port]},
                             permanent, 5000, worker, [widgy_websockets_server]},

    TimeServer = {time,
                  {time, start_link, []},
                  permanent, 5000, worker, [time]},

    {ok, { {one_for_one, 5, 10}, [WidgyWebsocketsServer, TimeServer]} }.

