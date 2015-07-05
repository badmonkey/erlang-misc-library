
-module({{supervisorid}}).
-vsn("{{version}}").

-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start() ->
    application:ensure_all_started({{name}}),
    lager:info("Started {{name}} server"),
    application:load({{name}}).
    
    
start_link() ->
    ?START_SUPERVISOR( {{supervisorid}} ).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init({{supervisorid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ {{serverid}}
            ] )      
      }
    }.
    

