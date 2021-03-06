-module(time).
-behaviour(gen_widget).

-export([get_state/0, get_config_options/0]).

get_config_options() ->
    [{location, [<<"GMT">>]}].

get_state() ->
    {{_,_,_},{Hour,Minute,Second}} = calendar:now_to_universal_time(now()),
    [{hour, Hour}, {minute, Minute}, {second, Second}].
