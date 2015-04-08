

%%%%% ------------------------------------------------------- %%%%%
% Helper macro for declaring supervisors and the children of supervisors



-define(SERVICE_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
-define(SERVICE_SPEC(I, Args), ?SERVICE_SPEC(I, I, Args)).
-define(SERVICE_SPEC(I), ?SERVICE_SPEC(I, I, [])).


-define(WORKER_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, temporary , brutal_kill, worker, [Mod]}).
-define(WORKER_SPEC(I, Args), ?WORKER(_SPECI, I, Args)).
-define(WORKER_SPEC(I), ?WORKER_SPEC(I, I, [])).


-define(CHILD_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, transient, 5000, worker, [Mod]}).
-define(CHILD_SPEC(I, Args), ?CHILD_SPEC(I, I, Args)).
-define(CHILD_SPEC(I), ?CHILD_SPEC(I, I, [])).


-define(SUPERVISOR_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, 5000, supervisor, [Mod]}).
-define(SUPERVISOR_SPEC(I, Args), ?SUPERVISOR_SPEC(I, I, Args)).
-define(SUPERVISOR_SPEC(I), ?SUPERVISOR_SPEC(I, I, [])).

-define(CHILDVISOR_SPEC(Id, Args), SUPERVISOR_SPEC(Id, ?MODULE, Args)).
-define(CHILDVISOR_SPEC(Id), ?SUPERVISOR_SPEC(Id, ?MODULE, [])).


-define(MODULE_SPEC(Mod, Id, Args), Mod:child_spec(Id, Args)).
-define(MODULE_SPEC(Mod, Id), ?MODULE_SPEC(Mod, Id, [])).
-define(MODULE_SPEC(Mod), ?MODULE_SPEC(Mod, Mod, [])).



-define(WORKER_SUPERVISOR(Id, Mod, Args, R, P), { {simple_one_for_one, R, P}, [?WORKER_SPEC(Id, Mod, Args)] }).
-define(WORKER_SUPERVISOR(I, R, P), ?WORKER_SUPERVISOR(I, I, [], R, P)).


-define(START_SUPERVISOR(Id), supervisor:start_link({local, Id }, ?MODULE, Id) ).


