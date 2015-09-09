
-module(erlx_frontloader_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(erlx_frontloader),
    lager:info("Started erlx_frontloader server"),
    application:load(erlx_frontloader).
    
    
start_link() ->
    ?START_SUPERVISOR( erlx_frontloader_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(erlx_frontloader_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ erlx_frontloader
            ] )      
      }
    }.
    

