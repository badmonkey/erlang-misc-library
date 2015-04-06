
-module({{supervisorid}}).
-vsn("{{version}}").
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("erlangx/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( {{supervisorid}} ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init({{supervisorid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , [ ?SERVICE({{serverid}})
        , ?CHILDVISOR(another_local_supervisor)
        ]
      }
    }.
    

