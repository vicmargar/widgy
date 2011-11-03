-module(widgy_websockets_server).
-export([start_link/1, stop/0]).

start_link(Port) ->
    misultin:start_link([
                         {port, Port},
                         {loop, fun(Req) -> handle_http(Req, Port) end},
                         {ws_loop, fun(Ws) -> handle_websocket(Ws) end}
                        ]).
stop() ->
    misultin:stop().

handle_http(Req, Port) ->
    Req:ok([{"Content-Type", "text/html"}],
    ["<html><head></head><body>Hello</body></html>"]).

handle_websocket(Ws) ->
    receive
        {browser, Data} ->
            Ws:send(["received '", Data, "'"]),
            handle_websocket(Ws);
        _Ignore ->
            handle_websocket(Ws)
    after 5000 ->
        Ws:send("pushing!"),
        handle_websocket(Ws)
    end.
