
-module(erlx_frontloader_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/0, start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(erlx_frontloader),
    lager:set_loglevel(lager_console_backend, debug),
    lager:info("Started erlx_frontloader server"),
    application:load(erlx_frontloader).


start(_StartType, _StartArgs) ->
    erlx_frontloader_sup:start_link().


stop(_State) ->
    ok.
    
