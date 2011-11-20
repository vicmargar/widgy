-module(dashboards).

-compile(export_all).

dash(Ctx) ->
    Dashboards = mustache:get(dashboards, Ctx),
    [ dict:from_list([{dashboard_id, D}]) || D <- Dashboards].
