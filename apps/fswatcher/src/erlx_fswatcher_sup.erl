
-module(erlx_fswatcher_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API

    
start_link() ->
    ?START_SUPERVISOR( erlx_fswatcher_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(erlx_fswatcher_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ fswatcher
            ] )      
      }
    }.
    

