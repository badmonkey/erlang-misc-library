
-module({{supid}}).
-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).



%%%%% ------------------------------------------------------- %%%%%
% Helper macro for declaring children of supervisor

-define(SERVICE(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(SERVICE(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
-define(SERVICE(Id, I, Args), {Id, {I, start_link, Args}, permanent, 5000, worker, [I]}).

-define(WORKER(I), {I, {I, start_link, []}, temporary , brutal_kill, worker, [I]}).

-define(CHILD(I), {I, {I, start_link, []}, transient, 5000, worker, [I]}).

-define(SUPERVISOR(I), {I, {I, start_link, []}, permanent, 5000, supervisor, [I]}).
-define(SUPERVISOR(I, Args), {I, {I, start_link, Args}, permanent, 5000, supervisor, [I]}).
-define(SUPERVISOR(Id, I, Args), {Id, {I, start_link, Args}, permanent, 5000, supervisor, [I]}).

-define(CHILDVISOR(N), {N, {?MODULE, start_link, N}, permanent, 5000, supervisor, [?MODULE]}).


-define(WORKER_POOL_SUP(I, R, P), { {simple_one_for_one, R, P}, [?WORKER(I)] }).



%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    supervisor:start_link({local, {{supid}} }, ?MODULE, {{supid}}).
    
    
start_link({{supid}}2) ->
    supervisor:start_link({local, {{supid}}_name }, ?MODULE, {{supid}}_pool).    


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor
    

init({{supid}}_pool) ->
    { ok, ?WORKER_POOL_SUP(replace_with_child_module, 2, 5) };
    
    
init({{supid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , [ ?SERVICE(replace_with_child_module)
        , ?CHILD(replace_with_child2_module)
        , ?SUPERVISOR(another_sup_module)
        , ?CHILDVISOR(another_sup_from_this_module)
        ]
      }
    }.
    

