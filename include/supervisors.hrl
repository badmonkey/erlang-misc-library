

%%%%% ------------------------------------------------------- %%%%%
% Helper macro for declaring supervisors and the children of supervisors

-ifndef(SHUTDOWN_TIMEOUT).
-define(SHUTDOWN_TIMEOUT, 5000).
-endif.


-define(GENERIC_SPEC(Id, Mod, Args, Restart, Type), {Id, {Mod, start_link, Args}, Restart, ?SHUTDOWN_TIMEOUT, Type, [Mod]}).


-define(SERVICE_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, ?SHUTDOWN_TIMEOUT, worker, [Mod]}).
-define(SERVICE_SPEC(I, Args), ?SERVICE_SPEC(I, I, Args)).
-define(SERVICE_SPEC(I), ?SERVICE_SPEC(I, I, [])).


-define(WORKER_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, temporary , brutal_kill, worker, [Mod]}).
-define(WORKER_SPEC(I, Args), ?WORKER(_SPECI, I, Args)).
-define(WORKER_SPEC(I), ?WORKER_SPEC(I, I, [])).


-define(CHILD_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, transient, ?SHUTDOWN_TIMEOUT, worker, [Mod]}).
-define(CHILD_SPEC(I, Args), ?CHILD_SPEC(I, I, Args)).
-define(CHILD_SPEC(I), ?CHILD_SPEC(I, I, [])).


-define(SUPERVISOR_SPEC(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, ?SHUTDOWN_TIMEOUT, supervisor, [Mod]}).
-define(SUPERVISOR_SPEC(I, Args), ?SUPERVISOR_SPEC(I, I, Args)).
-define(SUPERVISOR_SPEC(I), ?SUPERVISOR_SPEC(I, I, [])).

-define(CHILDVISOR_SPEC(Id, Args), SUPERVISOR_SPEC(Id, ?MODULE, Args)).
-define(CHILDVISOR_SPEC(Id), ?SUPERVISOR_SPEC(Id, ?MODULE, [])).


-define(MODULE_SPEC(Mod, Id, Args), Mod:child_spec(Id, Args)).
-define(MODULE_SPEC(Mod, Id), ?MODULE_SPEC(Mod, Id, [])).
-define(MODULE_SPEC(Mod), ?MODULE_SPEC(Mod, Mod, [])).



-define(WORKER_SUPERVISOR(Id, Mod, Args, R, P), { {simple_one_for_one, R, P}, [?WORKER_SPEC(Id, Mod, Args)] }).
-define(WORKER_SUPERVISOR(I, Args, R, P), ?WORKER_SUPERVISOR(I, I, Args, R, P)).
-define(WORKER_SUPERVISOR(I, R, P), ?WORKER_SUPERVISOR(I, I, [], R, P)).


-define(START_SUPERVISOR(Id), supervisor:start_link({local, Id}, ?MODULE, Id) ).


