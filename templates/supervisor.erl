
-module({{supid}}).
-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

-include("erlangx/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR( {{supid}} ).
    
    
start_link({{supid}}_id) ->
	?START_SUPERVISOR( {{supid}}_id ).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor
    

% clients can use this to start a new worker child in the worker group
% supervisor:start_child({{supid}}_group, [Args])

init({{supid}}_group) ->
    { ok, ?WORKER_GROUP_SUP(replace_with_child_module, 2, 5) };
    
    
init({{supid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , [ ?SERVICE(replace_with_child_module)
        , ?CHILD(replace_with_child2_module)
        , ?SUPERVISOR(another_sup_module)
        , ?CHILDVISOR({{supid}}_group)
        ]
      }
    }.
    

