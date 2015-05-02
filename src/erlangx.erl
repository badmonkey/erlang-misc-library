
-module(erlangx).

-behaviour(application).

-export([start/2, start/0, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application callbacks


start() ->
    application:load(erlangx).
    
start(_StartType, _StartArgs) ->
    {ok, spawn(
            fun() ->
                receive
                    _ -> ok
                end
            end )}.

stop(_State) ->
    ok.
