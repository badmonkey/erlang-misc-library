
-module(erlx_sysconfig_app).
-vsn("1.0.0").

-behaviour(application).

-export([start/2, stop/1]).


%%%%% ------------------------------------------------------- %%%%%
% Public API


start(_StartType, _StartArgs) ->
    erlx_sysconfig_sup:start_link().


stop(_State) ->
    ok.
    
