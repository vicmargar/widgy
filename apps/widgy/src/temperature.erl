-module(temperature).
-behaviour(gen_widget).

-export([get_state/0]).

-include_lib("xmerl/include/xmerl.hrl").

get_state() ->

    %% TODO: Take a look at this
    %% This is necessary to handle badarg errors in lhttpc
    %% process_flag(trap_exit, true),

    Location = "London",
    URL = "http://www.google.com/ig/api?weather=" ++ Location,

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

    [{temperature, list_to_integer(Temp)}].
