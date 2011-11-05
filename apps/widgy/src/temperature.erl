-module(temperature).
-behaviour(gen_widget).

-export([get_state/0]).

get_state() ->
    [{temperature, 28}].
