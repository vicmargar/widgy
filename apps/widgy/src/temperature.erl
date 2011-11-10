-module(temperature).
-behaviour(gen_widget).

-export([get_state/0, get_config_options/0]).

-include_lib("xmerl/include/xmerl.hrl").

get_config_options() ->
    [{locations, [<<"London">>, <<"Madrid">>, <<"Oviedo">>]}].

get_state() ->

    %% TODO: Take a look at this
    %% This is necessary to handle badarg errors in lhttpc
    %% process_flag(trap_exit, true),

    Config = get_config_options(),
    Locations = proplists:get_value(locations, Config),

    lists:map(fun(Location) ->
                      Temperature = get_temperature_for_location(Location),
                      {Location, Temperature}
              end,
              Locations).

get_temperature_for_location(Location) ->
    URL = "http://www.google.com/ig/api?weather=" ++ binary_to_list(Location),

    %% TODO: Abstract HTTP calls in a separate module
    Temp = try lhttpc:request(URL, "GET", [], 1000) of
               {ok, {{200, "OK"},Headers,Body}} ->
                   { Xml, _Rest } = xmerl_scan:string(binary_to_list(Body)),
                   %% TODO: Handle empty response
                   [DataAttribute] = xmerl_xpath:string("/xml_api_reply/weather/current_conditions/temp_c/@data", Xml),
                   DataAttribute#xmlAttribute.value;
               {error, timeout} ->
                   {false, timeout, [], []};
               {error, nxdomain} ->
                   {false, nxdomain, [], []};
               {error, econnrefused} ->
                   {false, econnrefused, [], []};
               {error, invalid_url} ->
                   {false, invalid_url, [], []};
               Response ->
                   io:format("Unexpected Response: ~p~n", [Response]),
                   ""
           catch
               exit:{nxdomain, Trace} ->
                   {error, nxdomain};
               exit:{econnrefused, Trace} ->
                   {error, econnrefused};
               exit:badarg ->
                   {error, invalid_url};
               Class:Error ->
                   io:format("Unexpected Error: ~p:~p~n", [Class, Error])
           end,

    list_to_integer(Temp).
