
-module({{supid}}).
-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).



%%%%% ------------------------------------------------------- %%%%%
% Helper macro for declaring children of supervisor

-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
-define(WORKER(I), {I, {I, start_link, []}, permanent, brutal_kill, worker, [I]}).
-define(SUPERVISOR(I), {I, {I, start_link, []}, permanent, 5000, supervisor, [I]}).
-define(SUPERVISOR(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).


-define(WORKER_POOL(I, R, P), { {simple_one_for_one, R, P}, [?WORKER(I)] }).



%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    supervisor:start_link({local, {{supid}} }, ?MODULE, {{supid}}).
    
    
start_link({{supid}}2) ->
    supervisor:start_link({local, {{supid}}_name }, ?MODULE, {{supid}}_pool).    


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor
    

init({{supid}}_pool) ->
    { ok, ?WORKER_POOL(replace_with_child_module, 2, 5) }.
    
    
init({{supid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , [ ?CHILD(replace_with_child_module)
        , ?CHILD(replace_with_child2_module)
        ]
      }
    }.
    

