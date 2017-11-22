
-module(xmaps).

-export([takekeys/2, takepairs/2, mutate/2, with/2, without/2, get_and_then/3]).


%%%%% ------------------------------------------------------- %%%%%


%-spec get( K, #{ K => V }, D ) -> V | D.

%get(Key, Map, Default)
%       when is_map(Map)  ->
%    case maps:find(Key, Map) of
%        { ok, Value }   -> Value
%    ;   error           -> Default
%    end.
    

%%%%% ------------------------------------------------------- %%%%%


-spec takekeys( type:cardinal(), #{ K => _ } ) -> [K].
    
takekeys(0, _)            -> [];
takekeys(N, Map)
        when is_map(Map)  ->
    {_, R} =    maps:fold(
                      fun (_, _, {0, R1}) -> {0, R1}
                      ;   (K, _, {X, R2}) -> {X-1, [K | R2]}
                      end
                    , {N, []}
                    , Map),
    R.
    
    
%%%%% ------------------------------------------------------- %%%%%
    

-spec takepairs( type:cardinal(), #{ K => V } ) -> [{K, V}].
    
takepairs(0, _)           -> [];
takepairs(N, Map)
        when is_map(Map)  ->
    {_, R} =    maps:fold(
                      fun (_, _, {0, R1}) -> {0, R1}
                      ;   (K, V, {X, R2}) -> {X-1, [{K, V} | R2]}
                      end
                    , {N, []}
                    , Map),
    R.    
    

%%%%% ------------------------------------------------------- %%%%%


-spec mutate( type:mutator(K, V), #{ K => V } ) -> #{ K => V }.

mutate(Pred, Map)
        when is_function(Pred,2), is_map(Map)  ->
    maps:from_list( mutate(Pred, [], maps:to_list(Map)) ).
    
    
mutate(_, Acc, []) ->
    lists:reverse(Acc);
    
mutate(Pred, Acc, [{K, V} | Rest]) ->   
    case Pred(K, V) of
        remove          -> mutate(Pred, Acc, Rest)
    ;   {value, X}      -> mutate(Pred, [{K, X} | Acc], Rest)
    ;   {value, Kn, X}  -> mutate(Pred, [{Kn, X} | Acc], Rest)
    end.
    

%%%%% ------------------------------------------------------- %%%%%


-spec with( [K], #{ K => V } ) -> #{ K => V }.

with(Keys, Map) when is_list(Keys), is_map(Map) ->
    maps:from_list( xlists:keywith(Keys, 1, maps:to_list(Map) ) ).


-spec without( [K], #{ K => V } ) -> #{ K => V }.

without(Keys, Map) when is_list(Keys), is_map(Map) ->
    maps:from_list( xlists:keywithout(Keys, 1, maps:to_list(Map) ) ).


%%%%% ------------------------------------------------------- %%%%%


-spec get_and_then( K, #{ K => V }, fun( (V) -> T ) | fun( (K, V) -> T ) ) -> undefined | T.

get_and_then(Key, Map, Then)
        when is_map(Map), is_function(Then, 1) ->
    case maps:get(Key, Map, undefined) of
        undefined   -> undefined
    ;   X           -> Then(X)
    end;

get_and_then(Key, Map, Then)
        when is_map(Map), is_function(Then, 2) ->
    case maps:get(Key, Map, undefined) of
        undefined   -> undefined
    ;   X           -> Then(Key, X)
    end.


%%%%% ------------------------------------------------------- %%%%%
    

%merge(F, Map1, Map2) ->
%	lists:foldl(
%	  	fun(Y, X) ->
%				maps:merge(X, maps:map(fun(Key, Value) ->
%                           case maps:find(Key, X) of
%                              {ok, V} -> F(Key, V, Value);
%                              error    -> Value
%                           end
%                    end,
%                Y))
%        end,
%        Map1,
%        Map2).


