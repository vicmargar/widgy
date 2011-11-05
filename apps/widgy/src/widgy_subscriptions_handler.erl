%% @doc Handles client subscriptions to Widgets
%%
%% This gen_server is responsible for keeping track of which client is subscribed
%% to which widget and viceversa.

-module(widgy_subscriptions_handler).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([subscribe/2, notify_clients/1, list_clients/0, list_widgets/0]).

-define(SERVER, ?MODULE).

-define(CLIENTS_TABLE, clients_table).
-define(WIDGETS_TABLE, widgets_table).

-record(state, {}).

subscribe(Widget, Client) ->
    gen_server:call(?MODULE, {subscribe, Widget, Client}).

notify_clients(WidgetsState) ->
    gen_server:call(?MODULE, {notify_clients, WidgetsState}).

list_clients() ->
    gen_server:call(?MODULE, list_clients).

list_widgets() ->
    gen_server:call(?MODULE, list_widgets).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    ets:new(?WIDGETS_TABLE, [named_table, public, set]),
    ets:new(?CLIENTS_TABLE, [named_table, public, set]),
    {ok, #state{}}.

handle_call({subscribe, Widget, Client}, _From, State) ->
    update_widgets_table(Widget, Client),
    update_clients_table(Widget, Client),
    {reply, ok, State};

handle_call({notify_clients, WidgetsState}, _From, State) ->
    lists:foreach(fun({Widget, WidgetState}) ->
                      Clients = get_clients_for_widget(Widget),
                      notify_clients(Widget, WidgetState, Clients)
                  end,
                  WidgetsState),

    {reply, ok, State};

handle_call(list_clients, _From, State) ->
    Clients = ets:tab2list(?CLIENTS_TABLE),
    {reply, Clients, State};

handle_call(list_widgets, _From, State) ->
    Widgets = ets:tab2list(?WIDGETS_TABLE),
    {reply, Widgets, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_widgets_table(Widget, Client) ->
    Clients = get_clients_for_widget(Widget),
    NewClients = {Widget, [Client | Clients]},
    ets:insert(?WIDGETS_TABLE, NewClients).

update_clients_table(Widget, Client) ->
    ets:insert(?CLIENTS_TABLE, {Client, Widget}).

get_clients_for_widget(Widget) ->
    case ets:lookup(?WIDGETS_TABLE, Widget) of
        [] -> [];
        [{Widget, ExistingClients}] -> ExistingClients
    end.

notify_clients(Widget, WidgetState, Clients) ->
    lists:foreach(fun(Client) ->
                          widgy_handler:send(Client, [{widget, Widget} | WidgetState])
                  end,Clients).
