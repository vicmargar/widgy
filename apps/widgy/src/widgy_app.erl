-module(widgy_app).

-behaviour(application).

-export([start/2, stop/1]).

-export([setup/0]).

-include_lib("widgy/include/widgy.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?WIDGETS_TABLE = ets:new(?WIDGETS_TABLE, [named_table, public, set]),
    ?DASHBOARDS_TABLE = ets:new(?DASHBOARDS_TABLE, [named_table, public, set]),

    Res = widgy_sup:start_link(),

    timer:sleep(1000),
    setup(),
    Res.


stop(_State) ->
    ok.


setup() ->
    DashboardId = widgy:create_dashboard(20000),

    Counter1 = widgy:start_counter(),
    Counter2 = widgy:start_counter(),
    Counter3 = widgy:start_counter(),

    widgy:add_widget_to_dashboard(Counter1, DashboardId),
    widgy:add_widget_to_dashboard(Counter2, DashboardId),
    widgy:add_widget_to_dashboard(Counter3, DashboardId).

