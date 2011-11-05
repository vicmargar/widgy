-module(widgy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).
-export([send/2]).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        _Other -> {ok, Req, undefined_state}
    end.

handle(Req, State) ->
    PathInfo = cowboy_http_req:path_info(Req),
    io:format("Path Infor ~p~n", [PathInfo]),
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello world!">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Data}, Req, State) ->
    case string:tokens(binary_to_list(Data), ":") of
        ["subscribe", ModuleStr] ->
            Module = list_to_atom(ModuleStr),
            widgy_subscriptions_handler:subscribe(Module, self());
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
