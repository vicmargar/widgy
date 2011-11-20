-module(widgy).

-include_lib("widgy/include/widgy.hrl").

-compile(export_all).

%% Create a dashboard
create_dashboard(Frequency) ->
    {Guid, Pid} = widgy_dashboard_sup:create_dashboard(Frequency),
    ets:insert(?DASHBOARDS_TABLE, {Guid, Pid}),
    Guid.

start_counter() ->
    {Guid, Pid} = widgy_counter_sup:start_counter(),
    ets:insert(?WIDGETS_TABLE, {Guid, Pid}),
    Guid.

add_widget_to_dashboard(WidgetId, DashboardId) ->
    widgy_dashboard:add_widget(WidgetId, DashboardId).

subscribe_to_dashboard(Client, DashboardId) ->
    widgy_dashboard:add_client(Client, DashboardId).

get_widget_state(WidgetId) when is_list(WidgetId) ->
    WidgetPid = get_widget_pid(WidgetId),
    get_widget_state(WidgetPid);
get_widget_state(WidgetPid) when is_pid(WidgetPid) ->
    gen_server:call(WidgetPid, get_state).

get_dashboard_state(DashboardId) when is_list(DashboardId) ->
    DashboardPid = get_dashboard_pid(DashboardId),
    get_dashboard_state(DashboardPid);
get_dashboard_state(DashboardPid) when is_pid(DashboardPid)->
    gen_server:call(DashboardPid, get_state).

get_dashboard_pid(DashboardId) ->
    [{DashboardId, Pid}] = ets:lookup(?DASHBOARDS_TABLE, DashboardId),
    Pid.

get_widget_pid(WidgetId) ->
    [{WidgetId, Pid}] = ets:lookup(?WIDGETS_TABLE, WidgetId),
    Pid.
