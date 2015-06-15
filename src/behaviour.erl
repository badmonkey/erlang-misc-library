
-module(behaviour).

-export([assert/2, check_for/2, is_any_of/2, gather_behaviours/1]).



%%%%% ------------------------------------------------------- %%%%%


-spec assert( atom(), atom() ) -> ok | no_return().

assert(Module, Behave) ->
    case check_for(Module, Behave) of
        true    -> ok
    ;   false   -> throw( {error, {bad_behaviour, Module, Behave}} )
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec check_for( atom(), atom() ) -> boolean().

check_for(Module, Behave)
        when  is_atom(Module)
            , is_atom(Behave)  ->
    Behaviours = gather_behaviours(Module),
    lists:member(Behave, Behaviours).

    
%%%%% ------------------------------------------------------- %%%%%


-spec is_any_of( atom(), [atom()] ) -> [atom()].

is_any_of(Module, Behaviours)
        when  is_atom(Module)
            , is_list(Behaviours)  ->
    Behave = gather_behaviours(Module),
    Intersect = sets:intersection( sets:from_list(Behaviours), sets:from_list(Behave) ),
    sets:to_list(Intersect).


%%%%% ------------------------------------------------------- %%%%%


gather_behaviours(Module) when is_atom(Module) ->
    gather_behaviours( Module:module_info(attributes), [] ).
    

gather_behaviours([], Acc) ->
    Acc;

gather_behaviours([Hd | Rest], Acc) ->
    case Hd of
        {behaviour, Entries}    -> gather_behaviours(Rest, Acc ++ Entries)
    ;   _                       -> gather_behaviours(Rest, Acc)
    end.


%%%%% ------------------------------------------------------- %%%%%
