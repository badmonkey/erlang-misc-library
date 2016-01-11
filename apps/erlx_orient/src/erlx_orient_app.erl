
-module(erlx_orient_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/0, start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(erlx_orient),
    lager:info("Started erlx_orient server"),
    application:load(erlx_orient).
    

start(_StartType, _StartArgs) ->
    erlx_orient_sup:start_link().


stop(_State) ->
    ok.
    
