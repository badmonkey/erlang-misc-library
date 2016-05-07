
-module({{applicationid}}).

-behaviour(application).

-export([start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application callbacks

    
start(_StartType, _StartArgs) ->
    {ok, self()}.

stop(_State) ->
    ok.

