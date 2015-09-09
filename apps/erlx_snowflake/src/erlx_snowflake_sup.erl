
-module(erlx_snowflake_sup).
-vsn("1.0.0").

-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started(snowflake),
    lager:info("Started erlx_snowflake server"),
    application:load(snowflake).
    
    
start_link() ->
    ?START_SUPERVISOR( erlx_snowflake_sup ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init(erlx_snowflake_sup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ snowflake
            ] )      
      }
    }.
    

