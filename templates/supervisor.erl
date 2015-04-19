
-module({{supervisorid}}).
-vsn("{{version}}").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlangx/include/supervisors.hrl").


%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    ?START_SUPERVISOR({{supervisorid}}).

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Supervisor

    
init({{supervisorid}}) ->
    { ok
    , { {one_for_one, 2, 5}
      , supervisor_child:build_specs(
            [ {{serverid}}
%            , MODULE_OR_TUPLE
            ] )
      }
    }.

%
% MODULE_OR_TUPLE can be
% module            -> {module, module, []}
% {id, module}      -> {id, module, []}
% {module, Args}    -> {module, module, Args}
% {id, module, Args}
% {id, module, Args, permanent|temporary|transient, worker|supervisor}
% a full child_spec()
%


%%
% Alternatively you can use macros to be more explicit
%
%init({{supervisorid}}) ->
%    { ok
%    , { {one_for_one, 2, 5}
%      , [ ?SERVICE_SPEC(server_module)
%        , ?CHILD_SPEC(transient_worker_module)
%        , ?SUPERVISOR_SPEC(supervisor_module)
%        , ?CHILDVISOR_SPEC(id_for_a_second_supervisor)
%        ]
%      }
%    }.
    
