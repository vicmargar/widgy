-module(widgy_dashboard_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([create_dashboard/1, create_dashboard/2]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WidgyDashboard = {widgy_dashboard,
                      {widgy_dashboard, start_link, []},
                      transient, 5000, worker, [widgy_dashboard]},

    {ok, {{simple_one_for_one, 5, 10}, [WidgyDashboard]}}.

create_dashboard(FrequencyUpdate) ->
    Guid = uuid:new(),
    create_dashboard(Guid, FrequencyUpdate).

create_dashboard(Id, FrequencyUpdate) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Id, FrequencyUpdate]),
    {Id, Pid}.
