
-module(behaviour).

-export([check_for/2, assert/2]).

-ifndef(STRICT_BEHAVIOUR).
-define(STRICT_BEHAVIOUR, true).
-endif.


%%%%% ------------------------------------------------------- %%%%%


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
    Attrs = Module:module_info(attributes),
    case { ?STRICT_BEHAVIOUR, search_for_behaviour(Behave, Attrs, undefined) } of
        {true, undefined}   -> false
    ;   {false, undefined}  -> true
    ;   {_, What}           -> What
    end.
    

search_for_behaviour(Behave, [], What) ->
    What;
    
search_for_behaviour(Behave, [Hd | Rest], What) ->
    case Hd of
        {behaviour, Entries}    ->
            case lists:member(Behave, Entries) of
                true    -> true
            ;   false   -> search_for_behaviour(Behave, Rest, false)
            end
            
    ;   _                       ->
            search_for_behaviour(Behave, Rest, What)
    end.

    
%%%%% ------------------------------------------------------- %%%%%
