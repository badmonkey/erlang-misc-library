-module(libmisc).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:load(libmisc).
    
start(_StartType, _StartArgs) ->
    ignore.

stop(_State) ->
    ok.