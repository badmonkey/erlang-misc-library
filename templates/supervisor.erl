
-module({{supervisorid}}).
-vsn("{{version}}").

-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

-include("erlangx/supervisors.hrl").


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
    { ok, ?WORKER_GROUP_SUP(replace_with_worker_child_module, 2, 5) };
    
    
init({{supervisorid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , [ ?SERVICE(replace_with_service_child_module)
        , ?CHILD(replace_with_child_module_from_another_file)
        , ?SUPERVISOR(replace_with_supervisor_module_from_another_file)
        , ?CHILDVISOR({{groupid}})
        ]
      }
    }.
    

