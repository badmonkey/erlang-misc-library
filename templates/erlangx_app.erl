
-module({{applicationid}}).
-vsn("{{version}}").

-behaviour(application).

-export([start/0, start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started({{name}}),
    lager:info("Started {{name}} server"),
    application:load({{name}}).
    

start(_StartType, _StartArgs) ->
    {{supervisorid}}:start_link().


stop(_State) ->
    ok.
    
