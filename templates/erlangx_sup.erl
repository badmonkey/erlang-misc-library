
-module({{supervisorid}}).
-vsn("{{version}}").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( {{supervisorid}} ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init({{supervisorid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs([{{serverid}}])
      }
    }.
    

