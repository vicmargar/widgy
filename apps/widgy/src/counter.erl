-module(counter).
-behaviour(gen_widget).

-export([get_state/0]).

-include_lib("xmerl/include/xmerl.hrl").

get_state() ->
  {ok, WidgetState} = redis_conn:get("widgy::counter::1"),
  WidgetState.