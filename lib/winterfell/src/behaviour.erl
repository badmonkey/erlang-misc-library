
-module(behaviour).

-export([assert/2, check_for_module/1, check_for/2, is_any_of/2, gather_behaviours/1]).



%%%%% ------------------------------------------------------- %%%%%


-spec assert( module(), atom() ) -> ok | type:exception().

assert(Module, Behave) ->
    case check_for(Module, Behave) of
        true    -> ok
    ;   false   -> throw( {error, {bad_behaviour, Module, Behave}} )
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec check_for_module( module() ) -> ok | type:exception().

check_for_module(Module) ->
    case code:which(Module) of
        non_existing    -> throw( {error, {no_module, Module}} )
    ;   _               -> ok
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec check_for( module(), atom() ) -> boolean().

check_for(Module, Behave)
        when  is_atom(Module)
            , is_atom(Behave)  ->
    Behaviours = gather_behaviours(Module),
    lists:member(Behave, Behaviours).

    
%%%%% ------------------------------------------------------- %%%%%


-spec is_any_of( module(), [atom()] ) -> [atom()].

is_any_of(Module, Behaviours)
        when  is_atom(Module)
            , is_list(Behaviours)  ->
    Behave = gather_behaviours(Module),
    Intersect = sets:intersection( sets:from_list(Behaviours), sets:from_list(Behave) ),
    sets:to_list(Intersect).


%%%%% ------------------------------------------------------- %%%%%


gather_behaviours(Module) when is_atom(Module) ->
    check_for_module(Module),
    [ Entry || {behaviour, Entry} <- Module:module_info(attributes) ].


%%%%% ------------------------------------------------------- %%%%%
