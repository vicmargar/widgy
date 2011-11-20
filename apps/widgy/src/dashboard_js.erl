-module('dashboard_js').

-compile(export_all).

dashboard_id(Ctx) ->
    mustache:get(dashboard_id, Ctx).

widgets(Ctx) ->
    DashboardId = mustache:get(dashboard_id, Ctx),
    DashboardState = widgy:get_dashboard_state(DashboardId),
    [dict:from_list([{widget_id, Id}]) || {Id, _} <- DashboardState].
