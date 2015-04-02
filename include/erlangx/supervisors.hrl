

%%%%% ------------------------------------------------------- %%%%%
% Helper macro for declaring children of supervisor


-define(SERVICE(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
-define(SERVICE(I, Args), ?SERVICE(I, I, Args)).
-define(SERVICE(I), ?SERVICE(I, I, [])).


-define(SUPERVISOR(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, 5000, supervisor, [Mod]}).
-define(SUPERVISOR(I, Args), ?SUPERVISOR(I, I, Args)).
-define(SUPERVISOR(I), ?SUPERVISOR(I, I, [])).


-define(CHILD(I), {I, {I, start_link, []}, transient, 5000, worker, [I]}).


-define(CHILDVISOR(Id), {Id, {?MODULE, start_link, Id}, permanent, 5000, supervisor, [?MODULE]}).


-define(WORKER(I), {I, {I, start_link, []}, temporary , brutal_kill, worker, [I]}).
-define(WORKER_GROUP_SUP(I, R, P), { {simple_one_for_one, R, P}, [?WORKER(I)] }).


-define(START_SUPERVISOR(Id), supervisor:start_link({local, Id }, ?MODULE, Id) ).


