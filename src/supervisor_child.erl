
-module(supervisor_child).

-export([build_specs/1, build_specs/2]).

-include_lib("erlangx/include/supervisors.hrl").

-define(BEHAVIOURS, [supervisor_child, supervisor, gen_server, port_server, tcp_listener]).


-callback child_spec(SpecId :: atom(), Args :: list()) -> supervisor:child_spec().


%%%%% ------------------------------------------------------- %%%%%


build_specs(Modules) -> build_specs(Modules, bestguess).


-type build_type() :: strict | bestguess.
-spec build_specs( [atom() | tuple()], build_type() ) -> [supervisor:child_spec()] | type:exception().

build_specs(Modules, Type) ->
    [ get_spec(X, Type) || X <- Modules ].
    

get_spec(M, Type)
        when is_atom(M)     ->
    get_spec(M, M, [], Type);

get_spec({Mod, Args}, Type)
        when is_list(Args) orelse is_map(Args)  ->
    get_spec(Mod, Mod, Args, Type);

get_spec({Id, Mod}, Type)   ->
    get_spec(Id, Mod, [], Type);
    
get_spec({Id, Mod, Args}, Type) ->
    get_spec(Id, Mod, Args, Type);

get_spec({Id, Mod, Args, Restart, SpecType}, _Type) ->
    ?GENERIC_SPEC(Id, Mod, wrap_args(Args), Restart, SpecType);
    
get_spec(Spec, _Type) when is_tuple(Spec) ->
    Spec.
    
    
-spec get_spec( atom(), atom(), list()|map(), atom() ) -> supervisor:child_spec() | type:exception().
    
get_spec(Id, Module, Args, Type)
        when  is_atom(Id)
            , is_atom(Module)
            , is_list(Args) orelse is_map(Args)
            , is_atom(Type)  ->
    Behaviours = behaviour:is_any_of(Module, ?BEHAVIOURS),
    IsSupChild = lists:member(supervisor_child, Behaviours),
    Other = other_behaviour(Behaviours),
    
    case {IsSupChild, Other, Type} of
        {true, _, _}            -> Module:child_spec(Id, Args)
    ;   {false, _, strict}      -> throw({error, {require_child_spec, Module}})
    ;   {false, undefined, _}   -> throw({error, {unknown_behaviour, Module}})
    ;   {false, supervisor, _}  -> ?SUPERVISOR_SPEC(Id, Module, wrap_args(Args))
    ;   {false, gen_server, _}  -> ?SERVICE_SPEC(Id, Module, wrap_args(Args))
    ;   {false, _, _}           -> ?SERVICE_SPEC(Id, Module, wrap_args(Args))
    end.
    
    
wrap_args(M) when is_map(M) -> [M];
wrap_args(L) when is_list(L) -> L.    

    
%%%%% ------------------------------------------------------- %%%%%
    
    
other_behaviour([]) ->
    undefined;
    
other_behaviour([supervisor_child | Rest]) ->
    other_behaviour(Rest);
    
other_behaviour([Hd | _Rest]) ->
    Hd.

