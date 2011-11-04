-module(widgy_websockets_server).
-export([start_link/1, stop/0, send/3]).

start_link(Port) ->
    misultin:start_link([
                         {port, Port},
                         {loop, fun(Req) -> handle_http(Req, Port) end},
                         {ws_loop, fun(Ws) -> handle_websocket(Ws) end}
                        ]).
stop() ->
    misultin:stop().

handle_http(Req, _Port) ->
    Req:ok([{"Content-Type", "text/html"}],
    ["<html><head></head><body>Hello</body></html>"]).

handle_websocket(Ws) ->
    receive
        {browser, Data} ->
            Ws:send(["received '", Data, "'"]),
            handle_data(Data, Ws),
            handle_websocket(Ws);
        _Ignore ->
            handle_websocket(Ws)
    after 5000 ->
        handle_websocket(Ws)
    end.

handle_data(Data, Ws) ->
    case string:tokens(Data, ":") of
        ["subscribe", ModuleStr] ->
            Module = list_to_atom(ModuleStr),
            Module:subscribe(Ws);
        Command -> io:format("ERROR - Unknown command: ~p~n", [Command])
    end.

send(Module, Ws, Params) ->
    NewParams = [ {widget, Module} | Params ],
    Ws:send(mochijson2:encode({struct, NewParams})).
