
-module(erlx_startphase_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/0, start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(erlx_startphase),
    lager:info("Started erlx_startphase server"),
    application:load(erlx_startphase).
    

start(_StartType, _StartArgs) ->
    erlx_startphase_sup:start_link().


stop(_State) ->
    ok.
    
