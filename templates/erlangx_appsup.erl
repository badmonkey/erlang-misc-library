
-module({{appsupid}}).
-vsn("{{version}}").

-behaviour(application).
-behaviour(supervisor).

-include_lib("erlangx/include/supervisors.hrl").

-export([start/0, start/2, stop/1]).
-export([start_link/0, init/1]).


%%%%% ------------------------------------------------------- %%%%%
% Application API


start() ->
    application:ensure_all_started({{name}}),
    lager:info("Started {{name}} server"),
    application:load({{name}}).
    

start(_StartType, _StartArgs) ->
    start_link().


stop(_State) ->
    ok.
    

%%%%% ------------------------------------------------------- %%%%%
% Supervisor API    
    
    
start_link() ->
    ?START_SUPERVISOR( {{appsupid}} ).
    
    
init({{appsupid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ {{serverid}}
            ] )      
      }
    }.
    

    