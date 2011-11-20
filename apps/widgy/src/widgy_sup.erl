-module(widgy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WIDGET(Type, Frequency), {Type, {gen_widget, start_link, [Type, Frequency]}, permanent, 5000, worker, [gen_widget]}).

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

    WidgyCounterSup = {widgy_counter_sup,
                       {widgy_counter_sup, start_link, []},
                       permanent, 5000, supervisor, [widgy_counter_sup]},

    WidgyDashboardSup = {widgy_dashboard_sup,
                         {widgy_dashboard_sup, start_link, []},
                         permanent, 5000, supervisor, [widgy_dashboard_sup]},

    WidgyServer = {widgy_server,
                   {widgy_server, start_link, [Port]},
                   permanent, 5000, worker, [widgy_server]},

    WidgySubscriptionsHandler = {widgy_subscriptions_handler,
                             {widgy_subscriptions_handler, start_link, []},
                             permanent, 5000, worker, [widgy_subscriptions_handler]},

    WidgyDataGatherer = {widgy_data_gatherer,
                             {widgy_data_gatherer, start_link, []},
                             permanent, 5000, worker, [widgy_data_gatherer]},

    TimeServer = ?WIDGET(time, 1000),
    TemperatureServer = ?WIDGET(temperature, 60000),

    {ok, { {one_for_one, 5, 10},
           [WidgyCounterSup,
            WidgyDashboardSup,
            WidgyServer
            %%WidgySubscriptionsHandler,
            %%WidgyDataGatherer,
            %%TimeServer,
            %%TemperatureServer
           ]
         }
    }.

