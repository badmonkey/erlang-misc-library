
-module({{applicationid}}).
-vsn("{{version}}").

-behaviour(application).

-export([start/0, start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:load({{name}}).
    
    
start(_StartType, _StartArgs) ->
    {{supervisorid}}:start_link().


stop(_State) ->
    ok.
    
