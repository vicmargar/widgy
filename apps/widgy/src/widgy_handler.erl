-module(widgy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).
-export([send/2]).

-include_lib("widgy/include/widgy.hrl").

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        _Other -> {ok, Req, undefined_state}
    end.

handle(Req, State) ->
    {Path, Req} = cowboy_http_req:path(Req),
    {Method, Req} = cowboy_http_req:method(Req),
    {Code, Response} = handle_request(Method, Path),
    {ok, Req2} = cowboy_http_req:reply(Code, [], Response, Req),
    {ok, Req2, State}.

handle_request(_, [<<"widgets">>, Widget]) ->
    StrWidget = binary_to_list(Widget),
    WidgetName = list_to_atom(StrWidget),
    case lists:member(WidgetName, ?WIDGETS) of
        true -> {200, mochijson2:encode({struct, [{config, {struct, WidgetName:get_config_options()}} | WidgetName:get_state()]})};
        _ -> {404, "Not Found"}
    end;

handle_request('GET', [<<"dashboards">>]) ->
    Dashboards = widgy:get_dashboards(),
    TFun = mustache:compile(dashboards),
    Ctx = dict:from_list([{dashboards, Dashboards}]),
    Response = mustache:render(dashboards, TFun, Ctx),
    {200, Response};

handle_request('GET', [<<"dashboards">>, DashboardId]) ->
    DashboardIdStr = binary_to_list(DashboardId),
    TFun = mustache:compile(dashboard),
    Ctx = dict:from_list([{dashboard_id, DashboardIdStr}]),
    Response = mustache:render(dashboard, TFun, Ctx),
    {200, Response};

handle_request('GET', [<<"js">>, <<"dashboards">>, DashboardId]) ->
    DashboardIdStr = binary_to_list(DashboardId),
    TFun = mustache:compile('dashboard_js'),
    Ctx = dict:from_list([{dashboard_id, DashboardIdStr}]),
    Response = mustache:render('dashboard_js', TFun, Ctx),
    {200, Response};

handle_request('GET', [<<"api">>, <<"dashboards">>]) ->
    DashboardsTable = ets:tab2list(?DASHBOARDS_TABLE),
    GUIDS = lists:map(fun({Guid, _}) ->
                              list_to_binary(Guid)
                         end, DashboardsTable),
    JSON = mochijson2:encode(GUIDS),
    {200, JSON};

handle_request('GET', [<<"api">>, <<"dashboards">>, DashboardId]) ->
    DashboardIdStr = binary_to_list(DashboardId),
    DashboardState = widgy:get_dashboard_state(DashboardIdStr),
    JSON = mochijson2:encode({struct, DashboardState}),
    {200, JSON};

handle_request('GET', [<<"api">>, <<"counters">>]) ->
    CountersTable = ets:tab2list(?COUNTERS_TABLE),
    Counters = lists:map(fun({Guid, _}) ->
                                 {Guid, {struct, widgy_counter:get_state(Guid)}}
                         end, CountersTable),
    JSON = mochijson2:encode({struct, Counters}),
    {200, JSON};

handle_request('POST', [<<"api">>, <<"counters">>]) ->
    Guid = widgy:start_counter(),
    {200, Guid};

handle_request('GET', [<<"api">>, <<"counters">>, CounterId]) ->
    Guid = binary_to_list(CounterId),
    CounterState = widgy_counter:get_state(Guid),
    {200, mochijson2:encode({struct, CounterState})};

handle_request('POST', [<<"api">>, <<"counters">>, CounterId]) ->
    Guid = binary_to_list(CounterId),
    NewCount = widgy_counter:increment(Guid),
    {200, integer_to_list(NewCount)};

handle_request(_,_) ->
    {404, "Not Found"}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Data}, Req, State) ->
    case string:tokens(binary_to_list(Data), ":") of
        ["subscribe"] ->
            widgy:subscribe_to_dashboard(self(), default);
        ["subscribe", DashboardId] ->
            widgy:subscribe_to_dashboard(self(), DashboardId);
        Command ->
            io:format("ERROR - Unknown command: ~p~n", [Command])
    end,
    {ok, Req, State};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Params}, Req, State) ->
    Data = mochijson2:encode({struct, Params}),
    {reply, {text, Data}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

send(Client, Params) ->
    Client ! {send, Params}.
