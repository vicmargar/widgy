-module(gen_widget).

-export([behaviour_info/1]).
-behaviour(gen_server).

-record(state, {widget_state, callback_module, timer}).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([send_state/1]).

behaviour_info(callbacks) ->
    [{get_state,0}];
behaviour_info(_Other) ->
    undefined.

send_state(Module) ->
    gen_server:cast(Module, send_state).

start_link(Module, Frequency) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module, Frequency], []).

init([Module, Frequency]) ->
    {ok, Timer} = timer:apply_interval(Frequency, ?MODULE, send_state, [Module]),
    {ok, #state{callback_module=Module, timer=Timer}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(send_state, State) ->
    Module = State#state.callback_module,
    WidgetState = Module:get_state(),
    widgy_data_gatherer:update_widget_state(Module, WidgetState),
    {noreply, State#state{widget_state=WidgetState}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
