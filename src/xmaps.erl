-module(xmaps).

-export([get/3]).


get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        { ok, Value }   -> Value
    ;   error           -> Default
    end.
    
