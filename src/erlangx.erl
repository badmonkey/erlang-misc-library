
-module(erlangx).

-behaviour(application).

-export([start/2, start/0, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application callbacks


start() ->
    application:load(erlangx).
    
start(_StartType, _StartArgs) ->
    erlangx_sup:start_link().

stop(_State) ->
    ok.
