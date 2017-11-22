
-module(frequency).

-export([ new/0, reset/2
        , increment/2, decrement/2, update/3
        , contains/2, value/2, as_map/1, as_list/1]).


%%%%% ------------------------------------------------------- %%%%%


new() ->
    {frequency, #{}}.

    
%%%%% ------------------------------------------------------- %%%%%


increment(Key, FreqMap) ->
    update(Key, +1, FreqMap).
    
decrement(Key, FreqMap) ->
    update(Key, -1, FreqMap).

    
%%%%% ------------------------------------------------------- %%%%%

    
reset(Key, {frequency, Map})
        when is_map(Map) ->
    {frequency, Map#{ Key => 0 }}.


%%%%% ------------------------------------------------------- %%%%%


update(Key, Amt, {frequency, Map})
        when is_number(Amt), is_map(Map) ->
    case maps:find(Key, Map) of
        error       -> {frequency, Map#{ Key => Amt }}
    ;   {ok, Value} -> {frequency, Map#{ Key => (Value + Amt) }}
    end.


%%%%% ------------------------------------------------------- %%%%%


contains(Key, {frequency, Map})
        when is_map(Map) ->
    case maps:find(Key, Map) of
        error   -> false
    ;   {ok, _} -> true
    end.


%%%%% ------------------------------------------------------- %%%%%


value(Key, {frequency, Map})     
        when is_map(Map) ->
    maps:get_value(Key, Map, 0).
    
    
as_map({frequency, Map}) when is_map(Map) ->
    Map.

as_list({frequency, Map}) when is_map(Map) ->
    maps:to_list(Map).

    