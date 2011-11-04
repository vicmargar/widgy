-module(widgy_websockets_server).
-export([start_link/1, stop/0]).

start_link(Port) ->
    Dispatch = [
        {'_', [
                {'_', widgy_handler, []}
            ]}
    ],
    cowboy:start_listener(widgy, 100,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).

stop() ->
    cowboy:stop(widgy).
