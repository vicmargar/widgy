%% @doc
%%
%% This gen_server is responsible for gathering data from all the widgets and
%% getting the list of clients from widgy_subscriptions_handler every second
%% to send the clients affected by a change their new data.

-module(widgy_data_gatherer).
-behaviour(gen_server).

-include("widgy/include/widgy.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([update_widget_state/2, notify_clients/0]).

-define(SERVER, ?MODULE).

-record(state, {widgets=[]}).

update_widget_state(Widget, State) ->
    gen_server:call(?MODULE, {update_widget_state, Widget, State}).

notify_clients() ->
    gen_server:call(?MODULE, notify_clients).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    timer:apply_interval(1000, ?MODULE, notify_clients, []),
    {ok, #state{}}.

handle_call({update_widget_state, Widget, WidgetState}, _From, State) ->
    %% prepend to the beginning of the list so when we do a lookup
    %% the first appearance of a widget is always the latest update.
    NewWidgetsState = [{Widget, Widget} | State#state.widgets],
    {reply, ok, State#state{widgets = NewWidgetsState}}.

handle_call(notify_clients, _From, State) ->
    UpdatedWidgets = State#state.widgets,
    LatestWidgetState = lists:map(
                          fun(W) ->
                                  %% This just returns the latest update
                                  %% at the front of the list
                                  case proplists:lookup(W, UpdatedWidgets) of
                                      none -> [];
                                      {W, LatestState} -> {W, LatestState}
                                  end
                          end,
                          ?WIDGETS),
    io:format("Notify clints:~p~n", [ LatestWidgetState ]),
    {reply, ok, State};

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
