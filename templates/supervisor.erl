
-module({{supervisorid}}).
-vsn("{{version}}").

-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( {{supervisorid}} ).
    
    
start_link({{groupid}}) ->
    ?START_SUPERVISOR( {{groupid}} ).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor


% clients can use this to start a new worker child in the worker group
% supervisor:start_child({{groupid}}, [Args])

init({{groupid}}) ->
    { ok, ?WORKER_SUPERVISOR(replace_with_worker_child_module, 2, 5) };
    
    
init({{supervisorid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , [ ?SERVICE_SPEC(replace_with_service_child_module)
        , ?CHILD_SPEC(replace_with_child_module_from_another_file)
        , ?SUPERVISOR_SPEC(replace_with_supervisor_module_from_another_file)
        , ?CHILDVISOR_SPEC({{groupid}})
        ]
      }
    }.
    

