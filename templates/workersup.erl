
-module({{supervisorid}}).
-vsn("{{version}}").

-behaviour(supervisor).

-export([start_link/0, start_child/1, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR({{supervisorid}}).
    
    
start_child(Args) ->    
    supervisor:start_child({{supervisorid}}, Args).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor


init({{supervisorid}}) ->
    { ok
    , ?GROUP_SUPERVISOR({{serverid}}, [], 2, 5)
    }.
    

