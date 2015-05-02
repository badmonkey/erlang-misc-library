
-module({{applicationid}}).

-behaviour(application).

-export([start/2, start/0, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application callbacks


start() ->
    application:load({{name}}).
    

start(_StartType, _StartArgs) ->
    {ok, spawn(
            fun() ->
                receive
                    _ -> ok
                end
            end )}.

stop(_State) ->
    ok.
