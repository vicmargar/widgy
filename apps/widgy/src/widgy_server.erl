-module(widgy_server).
-export([start_link/1, stop/0]).

-define(SERVER_HANDLE, widgy).

start_link(Port) ->
    Dispatch = [
        {'_', [
                {'_', widgy_handler, []}
            ]}
    ],
    cowboy:start_listener(?SERVER_HANDLE, 100,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).

stop() ->
    cowboy:stop(?SERVER_HANDLE).
