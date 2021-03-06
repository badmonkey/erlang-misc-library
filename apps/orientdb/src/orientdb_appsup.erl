
-module(orientdb_appsup).
-vsn("1.0.0").

-behaviour(application).
-behaviour(supervisor).

-include_lib("highgarden/include/supervisors.hrl").

-export([start/0, start/2, stop/1]).
-export([start_link/0, init/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application API


start() ->
    application:ensure_all_started(orientdb),
    lager:info("Started orientdb server"),
    application:load(orientdb).
    

start(_StartType, _StartArgs) ->
    start_link().


stop(_State) ->
    ok.
    

%%%%% ------------------------------------------------------- %%%%%
% Supervisor API    
    
    
start_link() ->
    ?START_SUPERVISOR(orientdb_appsup).
    
    
init(orientdb_appsup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ orient
            ] )      
      }
    }.
    

    