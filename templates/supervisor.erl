
-module({{supid}}).
-behaviour(supervisor).

-export([start_link/1, init/1]).



%%%%% ------------------------------------------------------- %%%%%
% Helper macro for declaring children of supervisor

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).


%%%%% ------------------------------------------------------- %%%%%
% Public API

-spec start_link(term())->{ok,pid()}|ignore|{error,any()}.
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor
    
init(_Args) ->
    Restart = {one_for_one, 2, 5},
    C0 = { arbitrary_internal_name_term
         , {mod,func,args}
         , permanent
         , 200 % ms
         , worker
         , [mod]
         },
    {ok,{Restart,[C0]}}.
    