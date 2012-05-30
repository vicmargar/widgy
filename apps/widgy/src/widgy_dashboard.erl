-module(widgy_dashboard).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_widget/2, add_client/2]).

-record(state, {guid, widgets=[], clients=[], dashboard_state=[]}).

add_widget(WidgetId, DashboardId) ->
    DashboardPid = widgy:get_dashboard_pid(DashboardId),
    gen_server:call(DashboardPid, {add_widget, WidgetId}).

add_client(ClientPid, DashboardId) ->
    DashboardPid = widgy:get_dashboard_pid(DashboardId),
    gen_server:call(DashboardPid, {add_client, ClientPid}).

start_link(Guid, Frequency) ->
    gen_server:start_link(?MODULE, [Guid, Frequency], []).

init([Guid, Frequency]) ->
    {ok, Timer} = timer:send_interval(Frequency, self(), send_updates),
    {ok, #state{guid=Guid}}.

handle_call(get_state, _From, State) ->
    Widgets = State#state.widgets,
    DashboardState = get_state(Widgets),
    {reply, DashboardState, State};

handle_call({add_widget, WidgetId}, _From, State) ->
    %%WidgetPid = widgy:get_widget_pid(WidgetId),
    Widgets = State#state.widgets,
    {reply, ok, State#state{widgets=[WidgetId | Widgets]}};

handle_call({add_client, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    {reply, ok, State#state{clients=[ClientPid | Clients]}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send_updates, State) ->
    Clients = State#state.clients,
    Widgets = State#state.widgets,

    io:format("Notifying clients: ~p of dashboard update, widgets: ~p~n", [Clients, Widgets]),

    DashboardState = get_state(Widgets),

    lists:foreach(fun(Client) ->
                          widgy_handler:send(Client, DashboardState)
                  end, Clients),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_state(Widgets) ->
    DashboardState = lists:map(fun(Id) ->
                                       {Id, widgy:get_widget_state(Id)}
                               end, Widgets),
    DashboardState.

