
-module(startdaemon_appsup).
-vsn("1.0.0").

-behaviour(application).
-behaviour(supervisor).

-include_lib("highgarden/include/supervisors.hrl").

-export([start/0, start/2, stop/1]).
-export([start_link/0, init/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application API


start() ->
    application:ensure_all_started(startdaemon),
    lager:info("Started startdaemon server"),
    application:load(startdaemon).
    

start(_StartType, _StartArgs) ->
    start_link().


stop(_State) ->
    ok.
    

%%%%% ------------------------------------------------------- %%%%%
% Supervisor API    
    
    
start_link() ->
    ?START_SUPERVISOR(startdaemon_appsup).
    
    
init(startdaemon_appsup) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ phase_server
            ] )      
      }
    }.
    

    