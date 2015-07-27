
-module(xmaps).
-extends(maps).

-export([get/3, takekeys/2, takepairs/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec get( K, #{ K => V }, D ) -> V | D.

get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        { ok, Value }   -> Value
    ;   error           -> Default
    end.
    

%%%%% ------------------------------------------------------- %%%%%


-spec takekeys( pos_integer(), #{ K => _ } ) -> [K].
    
takekeys(N, Map) ->
    {_, R} =    maps:fold(
                    fun (_, _, {0, R1}) -> {0, R1}
                    ;   (K, _, {X, R2}) -> {X-1, [K | R2]}
                    end,
                    {N, []},
                    Map),
    R.
    
    
%%%%% ------------------------------------------------------- %%%%%
    

-spec takepairs( pos_integer(), #{ K => V } ) -> [{K, V}].
    
takepairs(N, Map) ->
    {_, R} =    maps:fold(
                    fun (_, _, {0, R1}) -> {0, R1}
                    ;   (K, V, {X, R2}) -> {X-1, [{K, V} | R2]}
                    end,
                    {N, []},
                    Map),
    R.    
    
    